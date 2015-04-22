import operator

def read(path):
	timings_file = open(path, "r")
	data = timings_file.read()
	data = data.split("\n")

	measurements = {}

	for i in range(len(data)):
		if ("/ccs/home/csep04/codes/NekGPU15/nek5_r1039_acc/" in data[i]):
			function = data[i + 1].split()[0]
			time = data[i + 2].split()[1].replace(",", "")
			measurements[function] = float(time)

	# for key in measurements:
	# 	print "%30s - %15.3f" % (key, measurements[key])

	sorted_x = sorted(measurements.items(), key = operator.itemgetter(1))[::-1]

	for key, value in sorted_x:
		print "%30s - %15.3f" % (key, value)

	# measurements = []

	# with open(path, "r") as inF:
	# 	for line in inF:
	# 		if ("/ccs" in line):
	# 			print line

read("log_p8_g1")
