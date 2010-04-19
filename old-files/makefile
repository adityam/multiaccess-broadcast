PLOTFIGURE = 0 

FLAG=plotfigures : false 

ifeq ($(PLOTFIGURE), 1)
  FLAG=plotfigures : true  
endif


part1:
	maxima --batch-string="$(FLAG); batchload(\"part1.mac\");"

part2:
	maxima --batch-string="$(FLAG); batchload(\"part2.mac\");"

part3:
	maxima --batch-string="$(FLAG); batchload(\"part3.mac\");"


