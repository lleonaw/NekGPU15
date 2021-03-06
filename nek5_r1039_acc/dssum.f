      subroutine setupds(gs_handle,nx,ny,nz,nel,melg,vertex,glo_num)
      include 'SIZE'
      include 'INPUT'
      include 'PARALLEL'
      include 'NONCON'

      integer gs_handle
      integer vertex(1)
      integer*8 glo_num(1),ngv

      common /nekmpi/ mid,mp,nekcomm,nekgroup,nekreal

      t0 = dnekclock()

c     Global-to-local mapping for gs
      call set_vert(glo_num,ngv,nx,nel,vertex,.false.)

c     Initialize gather-scatter code
      ntot      = nx*ny*nz*nel
      call gs_setup(gs_handle,glo_num,ntot,nekcomm,mp)

c     call gs_chkr(glo_num)

      t1 = dnekclock() - t0
      if (nio.eq.0) then
         write(6,1) t1,gs_handle,nx,ngv,melg
    1    format('   setupds time',1pe11.4,' seconds ',2i3,2i12)
      endif
c
      return
      end
c-----------------------------------------------------------------------
      subroutine dssum(u,nx,ny,nz)
      include 'SIZE'
      include 'CTIMER'
      include 'INPUT'
      include 'NONCON'
      include 'PARALLEL'
      include 'TSTEP'
      real u(1)

      parameter (lface=lx1*ly1)
      common /nonctmp/ uin(lface,2*ldim),uout(lface)

      ifldt = ifield
c     if (ifldt.eq.0)       ifldt = 1
      if (ifldt.eq.ifldmhd) ifldt = 1
c     write(6,*) ifldt,ifield,gsh_fld(ifldt),imesh,' ifldt'

      if (ifsync) call nekgsync()

#ifndef NOTIMER
      if (icalld.eq.0) then
         tdsmx=0.
         tdsmn=0.
      endif
      icalld=icalld+1
      etime1=dnekclock()
#endif

c
c                 T         ~  ~T  T
c     Implement QQ   :=   J Q  Q  J
c 
c 
c                  T
c     This is the J  part,  translating child data
c
c      call apply_Jt(u,nx,ny,nz,nel)
c
c
c
c                 ~ ~T
c     This is the Q Q  part
c
      call gs_op(gsh_fld(ifldt),u,1,1,0)  ! 1 ==> +
c
c
c 
c     This is the J  part,  interpolating parent solution onto child
c
c      call apply_J(u,nx,ny,nz,nel)
c
c
#ifndef NOTIMER
      timee=(dnekclock()-etime1)
      tdsum=tdsum+timee
      ndsum=icalld
      tdsmx=max(timee,tdsmx)
      tdsmn=min(timee,tdsmn)
#endif
c
      return
      end
c-----------------------------------------------------------------------
      subroutine dsop(u,op,nx,ny,nz)
      include 'SIZE'
      include 'PARALLEL'
      include 'INPUT'
      include 'TSTEP'
      include  'CTIMER'

      real u(1)
      character*3 op
      character*10 s1,s2
c
c     o gs recognized operations:
c     
c             o "+" ==> addition.
c             o "*" ==> multiplication.
c             o "M" ==> maximum.
c             o "m" ==> minimum.
c             o "A" ==> (fabs(x)>fabs(y)) ? (x) : (y), ident=0.0.
c             o "a" ==> (fabs(x)<fabs(y)) ? (x) : (y), ident=MAX_DBL
c             o "e" ==> ((x)==0.0) ? (y) : (x),        ident=0.0.
c     
c             o note: a binary function pointer flavor exists.
c     
c     
c     o gs level:
c     
c             o level=0 ==> pure tree
c             o level>=num_nodes-1 ==> pure pairwise
c             o level = 1,...num_nodes-2 ==> mix tree/pairwise.
c      
c
      ifldt = ifield
c     if (ifldt.eq.0)       ifldt = 1
      if (ifldt.eq.ifldmhd) ifldt = 1

c     if (nio.eq.0) 
c    $   write(6,*) istep,' dsop: ',op,ifield,ifldt,gsh_fld(ifldt)

      if(ifsync) call nekgsync()

      if (op.eq.'+  ') call gs_op(gsh_fld(ifldt),u,1,1,0)
      if (op.eq.'sum') call gs_op(gsh_fld(ifldt),u,1,1,0)
      if (op.eq.'SUM') call gs_op(gsh_fld(ifldt),u,1,1,0)

      if (op.eq.'*  ') call gs_op(gsh_fld(ifldt),u,1,2,0)
      if (op.eq.'mul') call gs_op(gsh_fld(ifldt),u,1,2,0)
      if (op.eq.'MUL') call gs_op(gsh_fld(ifldt),u,1,2,0)

      if (op.eq.'m  ') call gs_op(gsh_fld(ifldt),u,1,3,0)
      if (op.eq.'min') call gs_op(gsh_fld(ifldt),u,1,3,0)
      if (op.eq.'mna') call gs_op(gsh_fld(ifldt),u,1,3,0)
      if (op.eq.'MIN') call gs_op(gsh_fld(ifldt),u,1,3,0)
      if (op.eq.'MNA') call gs_op(gsh_fld(ifldt),u,1,3,0)

      if (op.eq.'M  ') call gs_op(gsh_fld(ifldt),u,1,4,0)
      if (op.eq.'max') call gs_op(gsh_fld(ifldt),u,1,4,0)
      if (op.eq.'mxa') call gs_op(gsh_fld(ifldt),u,1,4,0)
      if (op.eq.'MAX') call gs_op(gsh_fld(ifldt),u,1,4,0)
      if (op.eq.'MXA') call gs_op(gsh_fld(ifldt),u,1,4,0)
c
      return
      end
c-----------------------------------------------------------------------
      subroutine vec_dssum(u,v,w,nx,ny,nz)
c
c     Direct stiffness summation of the face data, for field U.
c
c     Boundary condition data corresponds to component IFIELD of 
c     the CBC array.
c
      INCLUDE 'SIZE'
      INCLUDE 'TOPOL'
      INCLUDE 'INPUT'
      INCLUDE 'PARALLEL'
      INCLUDE 'TSTEP'
      include 'CTIMER'

      REAL U(1),V(1),W(1)

      if(ifsync) call nekgsync()

#ifndef NOTIMER
      if (icalld.eq.0) tvdss=0.0d0
      if (icalld.eq.0) tgsum=0.0d0
      icalld=icalld+1
      nvdss=icalld
      etime1=dnekclock()
#endif

c
c============================================================================
c     execution phase
c============================================================================
c
      ifldt = ifield
c     if (ifldt.eq.0)       ifldt = 1
      if (ifldt.eq.ifldmhd) ifldt = 1

      call gs_op_many(gsh_fld(ifldt),u,v,w,u,u,u,ndim,1,1,0)

#ifndef NOTIMER
      timee=(dnekclock()-etime1)
      tvdss=tvdss+timee
      tdsmx=max(timee,tdsmx)
      tdsmn=min(timee,tdsmn)
#endif

      return
      end

c-----------------------------------------------------------------------
      subroutine vec_dsop(u,v,w,nx,ny,nz,op)
c
c     Direct stiffness summation of the face data, for field U.
c
c     Boundary condition data corresponds to component IFIELD of 
c     the CBC array.
c
      INCLUDE 'SIZE'
      INCLUDE 'TOPOL'
      INCLUDE 'INPUT'
      INCLUDE 'PARALLEL'
      INCLUDE 'TSTEP'
      include 'CTIMER'
c
      real u(1),v(1),w(1)
      character*3 op

c============================================================================
c     execution phase
c============================================================================

      ifldt = ifield
c     if (ifldt.eq.0)       ifldt = 1
      if (ifldt.eq.ifldmhd) ifldt = 1

c     write(6,*) 'opdsop: ',op,ifldt,ifield
      if(ifsync) call nekgsync()

      if (op.eq.'+  ' .or. op.eq.'sum' .or. op.eq.'SUM')
     $   call gs_op_many(gsh_fld(ifldt),u,v,w,u,u,u,ndim,1,1,0)


      if (op.eq.'*  ' .or. op.eq.'mul' .or. op.eq.'MUL')
     $   call gs_op_many(gsh_fld(ifldt),u,v,w,u,u,u,ndim,1,2,0)


      if (op.eq.'m  ' .or. op.eq.'min' .or. op.eq.'mna'
     $                .or. op.eq.'MIN' .or. op.eq.'MNA')
     $   call gs_op_many(gsh_fld(ifldt),u,v,w,u,u,u,ndim,1,3,0)


      if (op.eq.'M  ' .or. op.eq.'max' .or. op.eq.'mxa'
     $                .or. op.eq.'MAX' .or. op.eq.'MXA')
     $   call gs_op_many(gsh_fld(ifldt),u,v,w,u,u,u,ndim,1,4,0)


      return
      end
c-----------------------------------------------------------------------
      subroutine nvec_dssum(u,stride,n,gs_handle)

c     Direct stiffness summation of the array u for n fields
c
      include 'SIZE'
      include 'CTIMER'

      real u(1)
      integer n,stride,gs_handle

      if(ifsync) call nekgsync()

#ifndef NOTIMER
      icalld=icalld+1
      nvdss=icalld
      etime1=dnekclock()
#endif
      call gs_op_fields(gs_handle,u,stride,n,1,1,0)

#ifndef NOTIMER
      timee=(dnekclock()-etime1)
      tvdss=tvdss+timee
      tdsmx=max(timee,tdsmx)
      tdsmn=min(timee,tdsmn)
#endif

      return
      end

c----------------------------------------------------------------------
      subroutine matvec3(uout,Jmat,uin,iftrsp,n1,n2)
c
      include 'SIZE'
c
      real Jmat (n1,n1,2)
      real uin   (1)
      real uout  (1)
      logical iftrsp
c
      common /matvtmp/ utmp(lx1,ly1)
c
      if (ndim.eq.2) then
         call mxm (Jmat(1,1,1),n1,uin,n1,uout,n2)
      else
         if (iftrsp) then
            call transpose(uout,n2,uin,n1)
         else
            call copy     (uout,uin,n1*n2)
         endif
         call mxm (Jmat(1,1,1),n1,uout,n1,utmp,n2)
         call mxm (utmp,n2,Jmat(1,1,2),n1,uout,n1)
      endif
c
      return
      end
c-----------------------------------------------------------------------
      subroutine matvec3t(uout,Jmat,uin,iftrsp,n1,n2)
c
      include 'SIZE'
c
      real Jmat (n1,n1,2)
      real uin   (n1,n2)
      real uout  (n1,n2)
      logical iftrsp
c
      common /matvtmp/ utmp(lx1*ly1)
c
      call transpose(utmp,n2,uin,n1)
      call mxm (Jmat(1,1,2),n1,utmp,n1,uout,n2)
      call mxm (uout,n2,Jmat(1,1,1),n1,utmp,n1)
      if (iftrsp) then
         call copy     (uout,utmp,n1*n2)
      else
         call transpose(uout,n2,utmp,n1)
      endif
c
      return
      end
c-----------------------------------------------------------------------
      subroutine matvect (out,d,vec,n1,n2)
      dimension d(n1,n2),out(1),vec(1)
c
c   handle non-square matrix in mat-vec mult -- TRANSPOSE
c    N1 is still the number of rows
c    N2 is still the number of cols
c
c
      call mxm(vec,1,d,n1,out,n2)
c
      return
      end
c-----------------------------------------------------------------------
c      subroutine opq_in_place(a,b,c)
c      include 'SIZE'
c      real a(1),b(1),c(1)
c
c      call q_in_place(a)
c      call q_in_place(b)
c      if (ndim .eq.3) call q_in_place(c)
c
c      return
c      end
c-----------------------------------------------------------------------
      subroutine vectof_add(b,a,ie,iface,nx,ny,nz)
C
C     Copy vector A to the face (IFACE) of B
C     IFACE is the input in the pre-processor ordering scheme.
C
      DIMENSION A(NX,NY)
      DIMENSION B(NX,NY,NZ,1)
      CALL FACIND (KX1,KX2,KY1,KY2,KZ1,KZ2,NX,NY,NZ,IFACE)
      k = 0
      DO 100 IZ=KZ1,KZ2
      DO 100 IY=KY1,KY2
      DO 100 IX=KX1,KX2
        k = k + 1
        B(IX,IY,IZ,IE) = B(IX,IY,IZ,IE) + A(k,1)
  100 CONTINUE
      return
      END
c-----------------------------------------------------------------------
      subroutine zero_f(b,ie,iface,nx,ny,nz)
C
C     ZERO the face (IFACE) of B
C     IFACE is the input in the pre-processor ordering scheme.
C
      DIMENSION B(NX,NY,NZ,1)
      CALL FACIND (KX1,KX2,KY1,KY2,KZ1,KZ2,NX,NY,NZ,IFACE)
c
      DO 100 IZ=KZ1,KZ2
      DO 100 IY=KY1,KY2
      DO 100 IX=KX1,KX2
        B(IX,IY,IZ,IE) = 0.
  100 CONTINUE
      return
      END
c-----------------------------------------------------------------------
      subroutine ftovec_0(a,b,ie,iface,nx,ny,nz)
C
C     Copy the face (IFACE) of B to vector A.
C     IFACE is the input in the pre-processor ordering scheme.
C
      DIMENSION A(NX,NY)
      DIMENSION B(NX,NY,NZ,1)
      CALL FACIND (KX1,KX2,KY1,KY2,KZ1,KZ2,NX,NY,NZ,IFACE)
      k = 0
      DO 100 IZ=KZ1,KZ2
      DO 100 IY=KY1,KY2
      DO 100 IX=KX1,KX2
        k = k + 1
        A(k,1)=B(IX,IY,IZ,IE)
        B(IX,IY,IZ,IE)=0.0
  100 CONTINUE
      return
      END
c-----------------------------------------------------------------------
      subroutine ftovec(a,b,ie,iface,nx,ny,nz)
C
C     Copy the face (IFACE) of B to vector A.
C     IFACE is the input in the pre-processor ordering scheme.
C
      real A(NX,NY)
      real B(NX,NY,NZ,1)
      CALL FACIND (KX1,KX2,KY1,KY2,KZ1,KZ2,NX,NY,NZ,IFACE)
      k = 0
      DO 100 IZ=KZ1,KZ2
      DO 100 IY=KY1,KY2
      DO 100 IX=KX1,KX2
        k = k + 1
        A(k,1)=B(IX,IY,IZ,IE)
  100 CONTINUE
      return
      END
c-----------------------------------------------------------------------
      subroutine vectof(b,a,ie,iface,nx,ny,nz)
C
C     Copy vector A to the face (IFACE) of B
C     IFACE is the input in the pre-processor ordering scheme.
C
      real A(NX,NY)
      real B(NX,NY,NZ,1)
      CALL FACIND (KX1,KX2,KY1,KY2,KZ1,KZ2,NX,NY,NZ,IFACE)
      k = 0
      DO 100 IZ=KZ1,KZ2
      DO 100 IY=KY1,KY2
      DO 100 IX=KX1,KX2
        k = k + 1
        B(IX,IY,IZ,IE) = A(k,1)
  100 CONTINUE
      return
      END
c-----------------------------------------------------------------------
      subroutine ftoveci(a,b,ie,iface,nx,ny,nz)
C
C     Copy the face (IFACE) of B to vector A.
C     IFACE is the input in the pre-processor ordering scheme.
C
      integer A(NX,NY)
      integer B(NX,NY,NZ,1)
      CALL FACIND (KX1,KX2,KY1,KY2,KZ1,KZ2,NX,NY,NZ,IFACE)
      k = 0
      DO 100 IZ=KZ1,KZ2
      DO 100 IY=KY1,KY2
      DO 100 IX=KX1,KX2
        k = k + 1
        A(k,1)=B(IX,IY,IZ,IE)
  100 CONTINUE
      return
      END
c-----------------------------------------------------------------------
      subroutine vectofi(b,a,ie,iface,nx,ny,nz)
C
C     Copy vector A to the face (IFACE) of B
C     IFACE is the input in the pre-processor ordering scheme.
C
      integer A(NX,NY)
      integer B(NX,NY,NZ,1)
      CALL FACIND (KX1,KX2,KY1,KY2,KZ1,KZ2,NX,NY,NZ,IFACE)
      k = 0
      DO 100 IZ=KZ1,KZ2
      DO 100 IY=KY1,KY2
      DO 100 IX=KX1,KX2
        k = k + 1
        B(IX,IY,IZ,IE) = A(k,1)
  100 CONTINUE
      return
      END
c-----------------------------------------------------------------------
      subroutine apply_Jt(u,nx,ny,nz,nel)
      include 'SIZE'
      include 'CTIMER'
      include 'INPUT'
      include 'NONCON'
      include 'PARALLEL'
      include 'TSTEP'
      real u(1)
c
      parameter (lface=lx1*ly1)
      common /nonctmp/ uin(lface,2*ldim),uout(lface)
c
c 
c                  T
c     This is the J  part,  translating child data
c
      do ie = 1 , nel
c        Note, we zero out u() on this face after extracting, for
c        consistency reasons discovered during Jerry's thesis. 
c        Thus,  "ftovec_0" rather than ftovec().   (iface -- Ed notation)
         do iface = 1 , 2*ndim
            im = mortar(iface,ie)
            if (im.ne.0) then
               call ftovec_0(uin(1,iface),u,ie,iface,nx,ny,nz)
            endif
         enddo
         do iface=1,2*ndim
            im = mortar(iface,ie)
            if (im.ne.0) then
               if (if3d) then
                 call matvec3t
     $               (uout,Jmat(1,1,1,im),uin(1,iface),ifJt(im),nx,nx)
               else
                 call matvect (uout,Jmat(1,1,1,im),uin(1,iface),nx,nx)
               endif
               call vectof_add(u,uout,ie,iface,nx,ny,nz)
            endif
         enddo
      enddo
c
      return
      end
c-----------------------------------------------------------------------
      subroutine apply_J(u,nx,ny,nz,nel)
      include 'SIZE'
      include 'CTIMER'
      include 'INPUT'
      include 'NONCON'
      include 'PARALLEL'
      include 'TSTEP'
      real u(1)
c
      parameter (lface=lx1*ly1)
      common /nonctmp/ uin(lface,2*ldim),uout(lface)
c
c     This is the J  part,  interpolating parent solution onto child
c
c
      do ie = 1 , nel
         do iface = 1 , 2*ndim
            im = mortar(iface,ie)
            if (im.ne.0) then
               call ftovec(uin(1,iface),u,ie,iface,nx,ny,nz)
            endif
         enddo
         do iface=1,2*ndim
            im = mortar(iface,ie)
            if (im.ne.0) then
               call matvec3
     $            (uout,Jmat(1,1,1,im),uin(1,iface),ifJt(im),nx,nz)
               call vectof (u,uout,ie,iface,nx,ny,nz)
            endif
         enddo
      enddo
c
      return
      end
c-----------------------------------------------------------------------
      subroutine h1_proj(u,nx,ny,nz)
      include 'SIZE'
      include 'CTIMER'
      include 'INPUT'
      include 'NONCON'
      include 'PARALLEL'
      include 'TSTEP'
      real u(1)
c
      parameter (lface=lx1*ly1)
      common /nonctmp/ uin(lface,2*ldim),uout(lface)

      if(ifsync) call nekgsync()

#ifndef NOTIMER
      if (icalld.eq.0) then
         tdsmx=0.
         tdsmn=0.
      endif
      icalld=icalld+1
      etime1=dnekclock()
#endif
c
      ifldt = ifield
c     if (ifldt.eq.0) ifldt = 1
      nel = nelv
      if (ifield.ge.2) nel=nelt
      ntot = nx*ny*nz*nel


c
c
c                        ~  ~T  
c     Implement   :=   J Q  Q  Mu
c 
c 
c                  T
c
      call col2  (u,umult,ntot)
c
c                 ~ ~T
c     This is the Q Q  part
c
      call gs_op(gsh_fld(ifldt),u,1,1,0) 
c
c 
c     This is the J  part,  interpolating parent solution onto child
c
      call apply_J(u,nx,ny,nz,nel)
c
c
#ifndef NOTIMER
      timee=(dnekclock()-etime1)
      tdsum=tdsum+timee
      ndsum=icalld
      tdsmx=max(timee,tdsmx)
      tdsmn=min(timee,tdsmn)
#endif
c
      return
      end
c-----------------------------------------------------------------------
      subroutine dssum_msk(u,mask,nx,ny,nz)
      include 'SIZE'
      include 'CTIMER'
      include 'INPUT'
      include 'NONCON'
      include 'PARALLEL'
      include 'TSTEP'
      real u(1),mask(1)
c
      parameter (lface=lx1*ly1)
      common /nonctmp/ uin(lface,2*ldim),uout(lface)

      if(ifsync) call nekgsync()

#ifndef NOTIMER
      if (icalld.eq.0) then
         tdsmx=0.
         tdsmn=0.
      endif
      icalld=icalld+1
      etime1=dnekclock()
#endif
c
      ifldt = ifield
c     if (ifldt.eq.0) ifldt = 1
      nel = nelv
      if (ifield.ge.2) nel=nelt
      ntot = nx*ny*nz*nel


c
c                    T           ~  ~T  T
c     Implement Q M Q   :=   J M Q  Q  J
c 
c 
c                  T
c     This is the J  part,  translating child data
c
      call apply_Jt(u,nx,ny,nz,nel)
c
c
c
c                 ~ ~T
c     This is the Q Q  part
c
      call gs_op(gsh_fld(ifldt),u,1,1,0) 
      call col2  (u,mask,ntot)
c
c 
c     This is the J  part,  interpolating parent solution onto child
c
      call apply_J(u,nx,ny,nz,nel)
c
c
#ifndef NOTIMER
      timee=(dnekclock()-etime1)
      tdsum=tdsum+timee
      ndsum=icalld
      tdsmx=max(timee,tdsmx)
      tdsmn=min(timee,tdsmn)
#endif
c
      return
      end
c-----------------------------------------------------------------------
      subroutine dssum_msk2(u,mask,binv,nx,ny,nz)
      include 'SIZE'
      include 'CTIMER'
      include 'INPUT'
      include 'NONCON'
      include 'PARALLEL'
      include 'TSTEP'
      real u(1),mask(1),binv(1)
c
      parameter (lface=lx1*ly1)
      common /nonctmp/ uin(lface,2*ldim),uout(lface)

      if(ifsync) call nekgsync()

#ifndef NOTIMER
      if (icalld.eq.0) then
         tdsmx=0.
         tdsmn=0.
      endif
      icalld=icalld+1
      etime1=dnekclock()
#endif

c
      ifldt = ifield
c     if (ifldt.eq.0) ifldt = 1
      nel = nelv
      if (ifield.ge.2) nel=nelt
      ntot = nx*ny*nz*nel


c
c
c                    T           ~  ~T  T
c     Implement Q M Q   :=   J M Q  Q  J
c 
c 
c                  T
c     This is the J  part,  translating child data
c
      call apply_Jt(u,nx,ny,nz,nel)
c
c
c
c                 ~ ~T
c     This is the Q Q  part
c
      call gs_op(gsh_fld(ifldt),u,1,1,0) 
      call col3  (u,mask,binv,ntot)
c
c 
c     This is the J  part,  interpolating parent solution onto child
c
      call apply_J(u,nx,ny,nz,nel)
c
c
#ifndef NOTIMER
      timee=(dnekclock()-etime1)
      tdsum=tdsum+timee
      ndsum=icalld
      tdsmx=max(timee,tdsmx)
      tdsmn=min(timee,tdsmn)
#endif
c
      return
      end
c-----------------------------------------------------------------------

#ifdef _OPENACC

      subroutine j8sort(a,ind,n) 
c     Sort routine for a = int*8, ind=int.
c     Uses heap sort (p 231 Num. Rec., 1st Ed.)

      integer*8 a(1),aa
      integer ind(1)

      do j=1,n
         ind(j)=j
      enddo

      if (n.le.1) return
      L=n/2+1
      ir=n
 100     continue
         if (l.gt.1) then
            l=l-1
            aa  = a  (l)
            ii  = ind(l)
         else
                 aa =   a(ir)
                 ii = ind(ir)
              a(ir) =   a( 1)
            ind(ir) = ind( 1)
            ir=ir-1
            if (ir.eq.1) then
                 a(1) = aa
               ind(1) = ii
               return
            endif
         endif
         i=l
         j=l+l
 200              continue
         if (j.le.ir) then
            if (j.lt.ir) then
               if ( a(j).lt.a(j+1) ) j=j+1
            endif
            if (aa.lt.a(j)) then
                 a(i) = a(j)
               ind(i) = ind(j)
               i=j
               j=j+j
            else
               j=ir+1
            endif
         goto 200
         endif
           a(i) = aa
         ind(i) = ii
         goto 100
      end

      subroutine setupds_hsmg_acc
     $     (l,gs_handle,nx,ny,nz,nel,melg,vertex,glo_num)
      include 'SIZE'
      include 'INPUT'
      include 'PARALLEL'
      include 'NONCON'

      integer gs_handle
      integer vertex(1)
      integer*8 glo_num(1),ngv
      integer l

      common /nekmpi/ mid,mp,nekcomm,nekgroup,nekreal

      t0 = dnekclock()

c     Global-to-local mapping for gs
      call set_vert(glo_num,ngv,nx,nel,vertex,.false.)

c     Initialize gather-scatter code
      ntot      = nx*ny*nz*nel
      call gs_setup(gs_handle,glo_num,ntot,nekcomm,mp)

      call gs_setup_hsmg_acc
     $     (l,glo_num,ngv,nx,ny,nz,nel,vertex,ifcenter,gs_handle)

c      call gs_chkr(glo_num)

      t1 = dnekclock() - t0
      if (nid.eq.0) then
         write(6,1) t1,gs_handle,nx,ngv,melg
 1           format('   setupds time',1pe11.4,' seconds ',2i3,2i12)
      endif
c
      return
      end

c-----------------------------------------------------------------------
      subroutine gs_setup_hsmg_acc
     $     (l,glo_num,ngv,nx,ny,nz,nel,vertex,ifcenter,gs_hnd)

      include 'SIZE'
      include 'TOTAL'
      include 'POPENACC'

c     Work arrays
   
      integer*8 ngv,glo_num(nx*ny*nz*nelv),wk(nx*ny*nz*nelv)
      real      u  (nx*ny*nz*nelv),v(nx*ny*nz*nelv)
      integer   idx(nx*ny*nz*nelv),gs_hnd
      integer   vertex ((2**ldim)*lelt)
      integer*8   pcount,  n_nonlocal, ie

      common /nekmpi/ mid,mp,nekcomm,nekgroup,nekreal

      logical ifcenter
      ifcenter = .false.

      param(66) = 4.   ! These give the std nek binary i/o and are 
      param(67) = 4.   ! good default values

c      call set_vert(glo_num,ngv,nx,nel,vertex,ifcenter)

      n = nx*ny*nz*nelv

      do ipass=1,2

         call i8copy(wk,glo_num,n)
         call j8sort(wk,idx,n)

         ig = 0
         ip = 0
         glo_num_last = 0

         do i=1,n
            if (wk(i).ne.0) then
              ip=ip+1
              il=idx(i)
              if (ig.eq.0) then
                 ig=ig+1
                 mg_lgl1(ip,l) = il
                 mg_lgl2(ip,l) = ig
              elseif (wk(i).eq.glo_num_last) then
                 mg_lgl1(ip,l) = il
                 mg_lgl2(ip,l) = ig
              else
                 ig = ig+1
                 mg_lgl1(ip,l) = il
                 mg_lgl2(ip,l) = ig
              endif
              glo_num_last  = wk(i)
              wk(i) = abs(wk(i))
            endif
         enddo
         mg_lgl1(0,l) = ip
         mg_lgl2(0,l) = ig
            
         ndssum      = mg_lgl1(0,l)
         nglobl      = mg_lgl2(0,l)

         if (ipass.eq.1) then ! eliminate singletons
c            call gs_setup(gs_hnd,glo_num,n,nekcomm,mp)
            call rone    (u,n)
            call gs_op   (gs_hnd,u,1,1,0)  ! 1 ==> +
            call gs_free (gs_hnd)
            call rone    (v,n)
            call ldssum_mg_acc  (v,l,wk)

            n_nonlocal = 0
            do i=1,ndssum              ! Identify singletons and nonlocals
               il=mg_lgl1(i,l)
               ig=mg_lgl2(i,l)
               if (u(il).lt.1.1) then          ! Singleton
                  glo_num(il) = 0
               elseif (u(il).ne.v(il)) then     ! Nonlocal
                  n_nonlocal  = n_nonlocal+1
                  glo_num(il) = -glo_num(il)
               endif
            enddo
         else                   ! ipass = 2

            igl = 0
            nnl = 0
            do i=1,n_nonlocal   ! Identify nonlocals in ug()
               ig=mg_lgl2(i,l)
               if (ig.ne.igl) nnl=nnl+1
               wk(nnl) = wk(i)
               igl = ig
            enddo

            n_nonlocal=nnl
            call gs_setup(gs_hnd,wk,n_nonlocal,nekcomm,mp)
c            gs_hnd(2) = n_nonlocal

         endif
      enddo

      mg_lgl1(-1,l) = gs_hnd
      mg_lgl2(-1,l) = n_nonlocal

C     Used in GPUs
      pcount = 1
      mg_ptr(1,l) = 1
      do i = 2, ndssum
         if (mg_lgl2(i,l) .ne. mg_lgl2(i-1,l)) then
            pcount = pcount+1
            mg_ptr(pcount,l) = i
         endif
      enddo
      mg_ptr(pcount+1,l) = ndssum + 1

C     check the result
      write(*,*) pcount, mg_lgl1(0,l), mg_lgl2(0,l), n_nonlocal

      if ( pcount .ne. mg_lgl2(0,l) ) then
         write(*,*) "mg_ptr is wrong"
         stop
      endif
             
      return
      end

c-----------------------------------------------------------------------
      subroutine hsmg_dssum_acc(u,l)
      include 'SIZE'
      include 'PARALLEL'
      include 'DSSUM'
      include 'POPENACC'

      integer nx,ny,nz

      parameter(lg=lx1*ly1*lz1*lelt)

c      real u(nx*ny*nz*lelt), ug(nx*ny*nz*lelt)
      real u(lg)

      common /ctmp2/ ug(lg)

      integer i,j,il,ig,gs_hnd

      integer ndssum, nglobl

      common /gsmpi/ gs_hnd

      ndssum=mg_lgl1(0,l)
      nglobl=mg_lgl2(0,l)
      gs_hnd      = mg_lgl1(-1,l)
      n_nonlocal  = mg_lgl2(-1,l)

!$ACC DATA PRESENT(u,mg_lgl1,mg_ptr)
!$ACC& PRESENT(ug(1:nglobl))
!$ACC PARALLEL LOOP COLLAPSE(1)
      do i = 1,nglobl
         ug(i) = 0.0
         ! local Q^T
!$ACC LOOP SEQ
         do j = mg_ptr(i,l),mg_ptr(i+1,l)-1
            il = mg_lgl1(j,l)
            ug(i) = ug(i) + u(il)
         enddo
      enddo
!$ACC END PARALLEL LOOP

      if (np.gt.1) then
cc         ngv = nv + n_nonlocal  ! Number that must be copied out
!$ACC UPDATE HOST(ug(1:n_nonlocal)) ASYNC(1)
!$ACC WAIT
         call gs_op(gs_hnd,ug(1),1,1,0)  ! 1 ==> +
!$ACC UPDATE DEVICE(ug(1:n_nonlocal)) ASYNC(2)
!$ACC WAIT
      endif

!$ACC PARALLEL LOOP COLLAPSE(1)
      do i = 1,nglobl
        ! local Q
!$ACC LOOP SEQ
         do j = mg_ptr(i,l),mg_ptr(i+1,l)-1
            il = mg_lgl1(j,l)
            u(il) = ug(i)
        enddo
      enddo
!$ACC END PARALLEL LOOP
!$ACC END DATA

      return 
      end

c-----------------------------------------------------------------------
      subroutine ldssum_mg_acc(u,l,ug)
      real u(1),ug(1)
      
      include 'SIZE'
      include 'DSSUM'
      include 'POPENACC'

      ndssum      = mg_lgl1(0,l)
      nglobl      = mg_lgl2(0,l)

      call rzero(ug,nglobl)

      do i=1,ndssum       ! local Q^T
         il=mg_lgl1(i,l)
         ig=mg_lgl2(i,l)
         ug(ig) = ug(ig)+u(il)
      enddo

      do i=1,ndssum       ! local Q
         il=mg_lgl1(i,l)
         ig=mg_lgl2(i,l)
         u(il) = ug(ig)
      enddo

      return
      end


c-----------------------------------------------------------------------
      subroutine vec_dssum_acc(u,v,w,nx,ny,nz)
c
c     Direct stiffness summation of the face data, for field U.
c
c     Boundary condition data corresponds to component IFIELD of 
c     the CBC array.
c
      INCLUDE 'SIZE'
      INCLUDE 'TOPOL'
      INCLUDE 'INPUT'
      INCLUDE 'PARALLEL'
      INCLUDE 'TSTEP'
      include 'CTIMER'

      REAL U(1),V(1),W(1)

      if(ifsync) call nekgsync()

#ifndef NOTIMER
      if (icalld.eq.0) tvdss=0.0d0
      if (icalld.eq.0) tgsum=0.0d0
      icalld=icalld+1
      nvdss=icalld
      etime1=dnekclock()
#endif

c
c============================================================================
c     execution phase
c============================================================================
c
      ifldt = ifield
c     if (ifldt.eq.0)       ifldt = 1
      if (ifldt.eq.ifldmhd) ifldt = 1

      call gs_op_many(gsh_fld(ifldt),u,v,w,u,u,u,ndim,1,1,0)

#ifndef NOTIMER
      timee=(dnekclock()-etime1)
      tvdss=tvdss+timee
      tdsmx=max(timee,tdsmx)
      tdsmn=min(timee,tdsmn)
#endif

      return
      end

c-----------------------------------------------------------------------                       
      subroutine gdssum_set_acc(glo_num,ngv,nx,nel,vertex,ifcenter)

      include 'SIZE'
      include 'TOTAL'
      include 'DSSUM'

c     Work arrays                                                                              

      integer*8 ngv,glo_num(nx1*ny1*nz1*nelv),wk(nx1*ny1*nz1*nelv)
      real      u  (nx1*ny1*nz1*nelv),v(nx1*ny1*nz1*nelv)
      integer   idx(nx1*ny1*nz1*nelv),gs_hnd(2)
      integer   vertex ((2**ldim)*lelt)
      integer*8   pcount,  n_nonlocal, ie

      common /nekmpi/ mid,mp,nekcomm,nekgroup,nekreal

      logical ifcenter
      ifcenter = .false.

      param(66) = 4.   ! These give the std nek binary i/o and are                             
      param(67) = 4.   ! good default values                                                   

c      call set_vert(glo_num,ngv,nx,nel,vertex,ifcenter)                                       

      n = nx1*ny1*nz1*nelv

      do ipass=1,2

         call i8copy(wk,glo_num,n)
         call j8sort(wk,idx,n)

         ig = 0
         ip = 0
         glo_num_last = 0
         do i=1,n
            if (wk(i).ne.0) then
              ip=ip+1
              il=idx(i)
              if (ig.eq.0) then
                 ig=ig+1
                 ids_lgl1(ip) = il
                 ids_lgl2(ip) = ig
              elseif (wk(i).eq.glo_num_last) then
                 ids_lgl1(ip) = il
                 ids_lgl2(ip) = ig
              else
                 ig = ig+1
                 ids_lgl1(ip) = il
                 ids_lgl2(ip) = ig
              endif
              glo_num_last  = wk(i)
              wk(i) = abs(wk(i))
            endif
         enddo
         ids_lgl1(0) = ip
         ids_lgl2(0) = ig

         ndssum      = ids_lgl1(0)
         nglobl      = ids_lgl2(0)
        if (ipass.eq.1) then ! eliminate singletons                                           
            call gs_setup(gs_hnd,glo_num,n,nekcomm,mp)
            call rone    (u,n)
            call gs_op   (gs_hnd,u,1,1,0)  ! 1 ==> +                                           
            call gs_free (gs_hnd)
            call rone    (v,n)
            call ldssum_acc (v,wk)

            n_nonlocal = 0
            do i=1,ndssum              ! Identify singletons and nonlocals                     
               il=ids_lgl1(i)
               ig=ids_lgl2(i)
               if (u(il).lt.1.1) then          ! Singleton                                     
                  glo_num(il) = 0
               elseif (u(il).ne.v(il)) then     ! Nonlocal                                     
                  n_nonlocal  = n_nonlocal+1
                  glo_num(il) = -glo_num(il)
               endif
            enddo
         else  ! ipass = 2                                                                     

            igl = 0
            nnl = 0
            do i=1,n_nonlocal   ! Identify nonlocals in ug()                                   
               ig=ids_lgl2(i)
               if (ig.ne.igl) nnl=nnl+1
               wk(nnl) = wk(i)
               igl = ig
            enddo

            n_nonlocal=nnl
            call gs_setup(gs_hnd,wk,n_nonlocal,nekcomm,mp)
            gs_hnd(2) = n_nonlocal

         endif
      enddo

      ids_lgl1(-1) = gs_hnd(1)
      ids_lgl2(-1) = gs_hnd(2)
C     Used in GPUs                                                                             
      pcount = 1
      ids_ptr(1) = 1
      do i = 2, ndssum
         if (ids_lgl2(i) .ne. ids_lgl2(i-1)) then
            pcount = pcount+1
            ids_ptr(pcount) = i
         endif
      enddo
      ids_ptr(pcount+1) = ndssum + 1

C     check the result                                                                         
      write(*,*) pcount, ids_lgl1(0), ids_lgl2(0), n_nonlocal

      if ( pcount .ne. ids_lgl2(0) ) then
         write(*,*) "ids_ptr is wrong"
         stop
      endif

      return
      end

c-----------------------------------------------------------------------
      subroutine ldssum_acc(u,ug)
      real u(1),ug(1)
      
      include 'SIZE'
      include 'DSSUM'

      ndssum      = ids_lgl1(0)
      nglobl      = ids_lgl2(0)

      call rzero(ug,nglobl)

      do i=1,ndssum       ! local Q^T
         il=ids_lgl1(i)
         ig=ids_lgl2(i)
         ug(ig) = ug(ig)+u(il)
      enddo

      do i=1,ndssum       ! local Q
         il=ids_lgl1(i)
         ig=ids_lgl2(i)
         u(il) = ug(ig)
      enddo

      return
      end
c-----------------------------------------------------------------------
      subroutine gdssum(u,nx,ny,nz)
      include 'SIZE'
      include 'PARALLEL'
      include 'DSSUM'

      parameter( lg=lx1*ly1*lz1*lelv)
      real u(lg),ug(lg)

      integer gs_hnd

      ndssum      = ids_lgl1(0)
      nglobl      = ids_lgl2(0)
      gs_hnd      = ids_lgl1(-1)
      n_nonlocal  = ids_lgl2(-1)

      call rzero(ug,nglobl)

      do i=1,ndssum       ! local Q^T
         il=ids_lgl1(i)
         ig=ids_lgl2(i)
         ug(ig) = ug(ig)+u(il)
      enddo

C No  implmented !!
c      if (mp.gt.1) then
c        call copyout(ug,n_nonlocal)
c         call gs_op(gs_hnd,ug,1,1,0)  ! 1 ==> +
c        call copyin (ug,n_nonlocal)
c      endif

      do i=1,ndssum       ! local Q
         il=ids_lgl1(i)
         ig=ids_lgl2(i)
         u(il) = ug(ig)
      enddo

      return
      end

c-----------------------------------------------------------------------
      subroutine dssum_acc(u,nx,ny,nz)
      include 'SIZE'
      include 'PARALLEL'
      include 'DSSUM'

      parameter(lg=lx1*ly1*lz1*lelt)

      real u(lg)

      common /ctmp2/ ug(lg)

      integer i,j,il,ig,gs_hnd

      integer ndssum, nglobl

      common /gsmpi/ gs_hnd

      ndssum=ids_lgl1(0)
      nglobl=ids_lgl2(0)
      gs_hnd      = ids_lgl1(-1)
      n_nonlocal  = ids_lgl2(-1)

!$ACC DATA PRESENT(ids_ptr(1:nglobl+1),u(1:lg))
!$ACC& PRESENT(ids_lgl1)
!$ACC& PRESENT(ug(1:nglobl))
!$ACC PARALLEL LOOP COLLAPSE(1)
      do i = 1,nglobl
         ug(i) = 0.0
         ! local Q^T
!$ACC LOOP SEQ
         do j = ids_ptr(i),ids_ptr(i+1)-1
            il = ids_lgl1(j)
            ug(i) = ug(i) + u(il)
         enddo
      enddo
!$ACC END PARALLEL LOOP

      if (np.gt.1) then
cc         ngv = nv + n_nonlocal  ! Number that must be copied out
!$ACC UPDATE HOST(ug(1:n_nonlocal)) ASYNC(3)
!$ACC WAIT
         call gs_op(gs_hnd,ug(1),1,1,0)  ! 1 ==> +
!$ACC UPDATE DEVICE(ug(1:n_nonlocal)) ASYNC(4)
!$ACC WAIT
      endif

!$ACC PARALLEL LOOP COLLAPSE(1)
      do i = 1,nglobl
        ! local Q
!$ACC LOOP SEQ
         do j = ids_ptr(i),ids_ptr(i+1)-1
            il = ids_lgl1(j)
            u(il) = ug(i)
        enddo
      enddo
!$ACC END PARALLEL LOOP
!$ACC END DATA

      return 
      end


c-----------------------------------------------------------------------
      subroutine gdssum2_acc(u,nx,ny,nz)
      include 'SIZE'
      include 'CTIMER'
      include 'INPUT'
      include 'NONCON'
      include 'PARALLEL'
      include 'TSTEP'

      parameter( lg=lx1*ly1*lz1*lelv)
      real u(lg)

      parameter (lface=lx1*ly1)
      common /nonctmp/ uin(lface,2*ldim),uout(lface)

      ifldt = ifield
c     if (ifldt.eq.0)       ifldt = 1
      if (ifldt.eq.ifldmhd) ifldt = 1
c     write(6,*) ifldt,ifield,gsh_fld(ifldt),imesh,' ifldt'

      if (ifsync) call nekgsync()

#ifndef NOTIMER
      if (icalld.eq.0) then
         tdsmx=0.
         tdsmn=0.
      endif
      icalld=icalld+1
      etime1=dnekclock()
#endif


      myflag = 1
      if (myflag.eq.0.and.istep.gt.0) then
c         myflag=ifield
c         call gdssum_set_acc
      endif
      if (myflag.eq.ifield) then
         call dssum_acc(u,nx,ny,nz)
         goto 100
      endif
      myflag = 1
      if (myflag.eq.0.and.istep.gt.0) then
c         myflag=ifield
c         call gdssum_set_acc
      endif
      if (myflag.eq.ifield) then
         call dssum_acc(u,nx,ny,nz)
         goto 100
      endif

c
c                 T         ~  ~T  T
c     Implement QQ   :=   J Q  Q  J
c 
c 
c                  T
c     This is the J  part,  translating child data
c
c      call apply_Jt(u,nx,ny,nz,nel)
c
c
c
c                 ~ ~T
c     This is the Q Q  part
c
      call gs_op(gsh_fld(ifldt),u,1,1,0)  ! 1 ==> +
c
c
c 
c     This is the J  part,  interpolating parent solution onto child
c
c      call apply_J(u,nx,ny,nz,nel)
c
c
 100    continue
#ifndef NOTIMER
      timee=(dnekclock()-etime1)
      tdsum=tdsum+timee
      ndsum=icalld
      tdsmx=max(timee,tdsmx)
      tdsmn=min(timee,tdsmn)
#endif
c
      return
      end

#endif 
