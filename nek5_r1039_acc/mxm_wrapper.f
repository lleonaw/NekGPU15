c-----------------------------------------------------------------------
      subroutine mxm(a,n1,b,n2,c,n3)
      use openacc    
c
c     Compute matrix-matrix product C = A*B
c     for contiguously packed matrices A,B, and C.
c
      real a(n1,n2),b(n2,n3),c(n1,n3)
      logical is_present

      is_present= ( acc_is_present(a,1).and.
     $              acc_is_present(b,1).and.
     $              acc_is_present(c,1) )
 
      if (is_present) then
!$ACC DATA PRESENT(a,b,c) if (is_present)
!$ACC PARALLEL LOOP if(is_present)
         do k=1,n3
         do i=1,n1
            c(i,k) = 0.
            do j=1,n2
                c(i,k) = c(i,k) + a(i,j)*b(j,k)
            enddo
         enddo
         enddo
!$ACC END PARALLEL LOOP
!$ACC END DATA
      else
         call mxm_cpu(a,n1,b,n2,c,n3)
      endif

      return
      end
c-----------------------------------------------------------------------
      subroutine mxm_cpu(a,n1,b,n2,c,n3)
c
c     Compute matrix-matrix product C = A*B
c     for contiguously packed matrices A,B, and C.
c
      real a(n1,n2),b(n2,n3),c(n1,n3)
c
      include 'SIZE'
      include 'OPCTR'
      include 'TOTAL'
c
      integer aligned
      integer K10_mxm

#ifndef NOTIMER
      if (isclld.eq.0) then
          isclld=1
          nrout=nrout+1
          myrout=nrout
          rname(myrout) = 'mxm   '
      endif
      isbcnt = n1*n3*(2*n2-1)
      dct(myrout) = dct(myrout) + (isbcnt)
      ncall(myrout) = ncall(myrout) + 1
      dcount      =      dcount + (isbcnt)
#endif

#ifdef BLAS_MXM
      call dgemm('N','N',n1,n3,n2,1.0,a,n1,b,n2,0.0,c,n1)
      return
#endif
 
#ifdef BG
      call bg_aligned3(a,b,c,aligned)
      if (n2.eq.2) then
         call mxm44_2(a,n1,b,n2,c,n3)
      else if ((aligned.eq.1) .and.
     $         (n1.ge.8) .and. (n2.ge.8) .and. (n3.ge.8) .and.
     $         (modulo(n1,2).eq.0) .and. (modulo(n2,2).eq.0) ) then
         if (modulo(n3,4).eq.0) then
            call bg_mxm44(a,n1,b,n2,c,n3)
         else
            call bg_mxm44_uneven(a,n1,b,n2,c,n3)
         endif
      else if((aligned.eq.1) .and.
     $        (modulo(n1,6).eq.0) .and. (modulo(n3,6).eq.0) .and.
     $        (n2.ge.4) .and. (modulo(n2,2).eq.0) ) then
         call bg_mxm3(a,n1,b,n2,c,n3)
      else
         call mxm44_0(a,n1,b,n2,c,n3)
      endif
      return
#endif

#ifdef K10_MXM
      ! fow now only supported for lx1=8
      ! tuned for AMD K10
      ierr = K10_mxm(a,n1,b,n2,c,n3) 
      if (ierr.gt.0) call mxmf2(a,n1,b,n2,c,n3)
      return
#endif


      call mxmf2(a,n1,b,n2,c,n3)

      return
      end
c-----------------------------------------------------------------------
