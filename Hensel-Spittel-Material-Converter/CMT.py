#!/usr/bin/python
# -*- coding: iso-8859-15 -*-

"""
#****.py
#
# FACTS :  #################################################################
#		
#	AUTHOR:     TRAUTH, Daniel (trt)
#	DATE:	    2013-03-28
#	VERSION:    130328_CMT.py
#
#       COMPANY:    Werkzeugmaschinenlabor WZL der RWTH Aachen
#		    RWTH Aachen	
#                   D-52056 Aachen
#
# OBJECTIVE : 
#	This python file is called by the Hensel-Spittel Material Converter Gui
#	in order to calculate table data using constitutive formulation of
#	the yield stress by means of the Hensel-Spittel-Approach given by
#	IBF of RWTH Aachen University or Springer Materials.
#
# CHANGELOG : ###############################################################
#
#               130328:
#              		- new: defining cmt function
#
"""

## Import Modules based on Abaqus Scripting User's Manual Example 1
## This statement makes the basic Abaqus objects accessible to the script. It also provides access to a default model database using 
## the variable named mdb. The statement, from abaqusConstants import *, makes the Symbolic Constants defined by the Abaqus 
## Scripting Interface available to the script.
from abaqus import *
from abaqusConstants import *
from datetime import datetime, date, time

# ## These statements provide access to the objects related to sketches and parts. sketch and part are called Python modules.
# import section
# import regionToolset
# import displayGroupMdbToolset as dgm
# import part
# import material
# import assembly
# import step
# import interaction
# import load
# import mesh
# import optimization
# import job
# import sketch
import visualization
import xyPlot
# import displayGroupOdbToolset as dgo
# import connectorBehavior

# backwardCompatibility.setValues(includeDeprecated=True, reportDeprecated=False)

# ## Import Modules by HWG
# import testUtils
# testUtils.setBackwardCompatibility()



# range() only support intergers. frange() is self-made range-function for floats	
def frange(start, stop, step):
	rc = []
	x = start
	while( x<=stop ):
		rc.append(x)
		x+=step
	return rc

def elapsedTime(starttime):	
	endtime = datetime.now()
	dauer = endtime - starttime
	print "... time elapsed in h:m:s:ms = %s" % (dauer)
	print '... done.'
	print ' '

def importMaterialParameters(hImportFileName, hHSA):
		print '>>> Importing material datasheet ...'
		import win32com.client
		from win32com.client import constants as c
		from win32com.client import Dispatch
		#print hImportFileName
		#f = open(hImportFileName)
		#print f
		try:
			xlApp = Dispatch('Excel.Application')
			xlApp.Workbooks.Open(hImportFileName)
			activeSheet = xlApp.ActiveSheet
			#print activeSheet
		except:
			print '****ERROR**** Excel-Interface'
		# Notation of the Excel-format
		# A1 = 1.1; B1 = 1.2
		# Cells(1.1) = A1 = Name of the file and the material. Should be identical
		# Cells(1.2) = B1 = Labeling of the material, e.g. S235JRG2
		# Cells(1.3) = C1 = Identifier of the material, e.g. 1.0038
		# Cells(1.4) = D1 = Hensel-Spittel-Parameter K/A
		# Cells(1.5) = E1 = Hensel-Spittel-Parameter m1
		# Cells(1.6) = F1 = Hensel-Spittel-Parameter m2
		# Cells(1.7) = G1 = Hensel-Spittel-Parameter m3
		# Cells(1.8) = H1 = Hensel-Spittel-Parameter m4
		# Cells(1.9) = I1 = Hensel-Spittel-Parameter m5
		# Cells(1.10) = J1 = Hensel-Spittel-Parameter m6
		# Cells(1.11) = K1 = Hensel-Spittel-Parameter m7
		# Cells(1.12) = L1 = Hensel-Spittel-Parameter m8
		# Cells(1.13) = M1 = Source of the material data
		# Cells(1.14) = N1 = Applicatiom, e.g. Cold deformation, Hot deformation
		# Cells(1.15) = O1 = State of Material, e.g. Soft annealed
		# Cells(1.16) = P1 = Material Description for the use in ABQ
		# Cells(1.17) = Q1 = Tag to verifiy if the material data sheet matches the converter-configuration
		
		if (hHSA == 'IBF(Cold)'):
			print '... Check for Tag-identifier: IBF(Cold)'
			#print activeSheet.Cells(2,17).Value
			if (activeSheet.Cells(2,17).Value != hHSA):
				print '... ... Tag not found. Check activated Hensel-Spittel-Approach or imported Excel-file'
				exit()
			else:
				print "... ... Found Tag. Importing parameters for %s" % (hHSA)
				datarow = 2
				hMaterialName = activeSheet.Cells(datarow,1).Value
				hMaterialDescription = activeSheet.Cells(datarow,16).Value
				hKIbfC = activeSheet.Cells(datarow,4).Value
				hM1IbfC = activeSheet.Cells(datarow,5).Value
				hM2IbfC = activeSheet.Cells(datarow,6).Value
				hM3IbfC = activeSheet.Cells(datarow,7).Value
				hM4IbfC = activeSheet.Cells(datarow,8).Value
				
				hMaterialLabel = activeSheet.Cells(datarow,2).Value
				hMaterialIndentifier = activeSheet.Cells(datarow,3).Value
				hMaterialSource = activeSheet.Cells(datarow,13).Value
				hMaterialApplication = activeSheet.Cells(datarow,14).Value
				hMaterialState = activeSheet.Cells(datarow,15).Value
				if hMaterialDescription == None:
					hMaterialDescription = "%s (%s) - %s %s steel. Generated with the Hensel-Spittel-Converter using constitutive Hensel-Spittel-Parameters determined by %s "%(hMaterialLabel,hMaterialIndentifier,hMaterialState,hMaterialApplication,hMaterialSource)
				
				print '... done.'
				return {
				'hHSA':activeSheet.Cells(2,17).Value,
				'hMaterialName':hMaterialName,
				'hMaterialDescription':hMaterialDescription,
				'hKIbfC':hKIbfC,
				'hM1IbfC':hM1IbfC,
				'hM2IbfC':hM2IbfC,
				'hM3IbfC':hM3IbfC,
				'hM4IbfC':hM4IbfC,
				}
		
		elif (hHSA == 'Springer-Materials(Cold)'):
			print '... Check for Tag-identifier: Springer-Materials(Cold)'
			if (activeSheet.Cells(2,17).Value != hHSA):
				print '... ... Tag not found. Check activated Hensel-Spittel-Approach or imported Excel-file'
				exit()
			else:
				print "... ... Found Tag. Importing parameters for %s" % (hHSA)
				datarow = 2
				hMaterialName = activeSheet.Cells(datarow,1).Value
				hMaterialDescription = activeSheet.Cells(datarow,16).Value
				hASmC = activeSheet.Cells(datarow,4).Value
				hM1SmC = activeSheet.Cells(datarow,5).Value
				hM2SmC = activeSheet.Cells(datarow,6).Value
				hM3SmC = activeSheet.Cells(datarow,7).Value
				hM4SmC = activeSheet.Cells(datarow,8).Value
				
				hMaterialLabel = activeSheet.Cells(datarow,2).Value
				hMaterialIndentifier = activeSheet.Cells(datarow,3).Value
				hMaterialSource = activeSheet.Cells(datarow,13).Value
				hMaterialApplication = activeSheet.Cells(datarow,14).Value
				hMaterialState = activeSheet.Cells(datarow,15).Value
				if hMaterialDescription == None:
					hMaterialDescription = "%s (%s) - %s %s steel. Generated with the Hensel-Spittel-Converter using constitutive Hensel-Spittel-Parameters determined by %s "%(hMaterialLabel,hMaterialIndentifier,hMaterialState,hMaterialApplication,hMaterialSource)
							
				print '... done.'
				return {
				'hHSA':activeSheet.Cells(2,17).Value,
				'hMaterialName':hMaterialName,
				'hMaterialDescription':hMaterialDescription,
				'hASmC':hASmC,
				'hM1SmC':hM1SmC,
				'hM2SmC':hM2SmC,
				'hM3SmC':hM3SmC,
				'hM4SmC':hM4SmC,
				}
		
		elif (hHSA == 'Springer-Materials(Hot)'):
			print '... Check for Tag-identifier: Springer-Materials(Hot)'
			if (activeSheet.Cells(2,17).Value != hHSA):
				print '... ... Tag not found. Check activated Hensel-Spittel-Approach or imported Excel-file'
				exit()
			else:
				print "... ... Found Tag. Importing parameters for %s" % (hHSA)
				datarow = 2
				hMaterialName = activeSheet.Cells(datarow,1).Value
				hMaterialDescription = activeSheet.Cells(datarow,16).Value
				hASmH = activeSheet.Cells(datarow,4).Value
				hM1SmH = activeSheet.Cells(datarow,5).Value
				hM2SmH = activeSheet.Cells(datarow,6).Value
				hM4SmH = activeSheet.Cells(datarow,8).Value
				hM5SmH = activeSheet.Cells(datarow,9).Value
				hM7SmH = activeSheet.Cells(datarow,11).Value
				hM8SmH = activeSheet.Cells(datarow,12).Value
				
				hMaterialLabel = activeSheet.Cells(datarow,2).Value
				hMaterialIndentifier = activeSheet.Cells(datarow,3).Value
				hMaterialSource = activeSheet.Cells(datarow,13).Value
				hMaterialApplication = activeSheet.Cells(datarow,14).Value
				hMaterialState = activeSheet.Cells(datarow,15).Value
				if hMaterialDescription == None:
					hMaterialDescription = "%s (%s) - %s %s steel. Generated with the Hensel-Spittel-Converter using constitutive Hensel-Spittel-Parameters determined by %s "%(hMaterialLabel,hMaterialIndentifier,hMaterialState,hMaterialApplication,hMaterialSource)
			
				print '... done.'
				return {
				'hHSA':activeSheet.Cells(2,17).Value,
				'hMaterialName':hMaterialName,
				'hMaterialDescription':hMaterialDescription,
				'hASmH':hASmH,
				'hM1SmH':hM1SmH,
				'hM2SmH':hM2SmH,
				'hM4SmH':hM4SmH,
				'hM5SmH':hM5SmH,
				'hM7SmH':hM7SmH,
				'hM8SmH':hM8SmH,
				}
		xlApp.Quit()
		del activeSheet
		xlApp = None
		del xlApp
		print '... done.'
		print ' '
		
def plotMaterial(hMaterialName,hHSA,myMaterialData,hConsiderTemperatures,hConsiderStrainRates,hTemperatures,hStrainrates):
	starttime = datetime.now()
	print '... plotting material data ...'
	#print hMaterialName
	xyp = session.XYPlot(hMaterialName)
	#print 'unicode error 0'
	chartName = xyp.charts.keys()[0]
	chart = xyp.charts[chartName]
	xQuantity = visualization.QuantityType(type=STRAIN)
	yQuantity = visualization.QuantityType(type=STRESS)
	#print 'unicode error 1'
	if (hConsiderStrainRates != True) and (hConsiderTemperatures != True):
		xy1 = xyPlot.XYData(name="%s"%(hMaterialName), data=(myMaterialData), 
			sourceDescription='Entered from keyboard', axis1QuantityType=xQuantity, 
			axis2QuantityType=yQuantity, )
		#xy2 = xyPlot.XYData(data=((300,800),(1,2)), 
		#	sourceDescription='Entered from keyboard', axis1QuantityType=xQuantity, 
		#	axis2QuantityType=yQuantity, )
		c1 = session.Curve(name="%s_RT"%(hMaterialName), xyData=xy1)
		#c2 = session.Curve(xyData=xy2)
		chart.setValues(curvesToPlot=(c1, ), appendMode=True )
		#chart.setValues(curvesToPlot=(c2, ), appendMode=True )
		
	if ((hConsiderStrainRates == True) and (hConsiderTemperatures != True)):
		myStrainratesNumber = len(hStrainrates)
		for rate in range(0,myStrainratesNumber,1):
			strainrate = hStrainrates[rate][0]
			list = [x[:-1] for x in myMaterialData if x[2] == strainrate]
			#print strainrate
			#print list[5]
			xy1 = xyPlot.XYData(name="%s_%s"%(hMaterialName,strainrate), data=(list), 
				sourceDescription='Entered from keyboard', axis1QuantityType=xQuantity, 
				axis2QuantityType=yQuantity, )
			c1 = session.Curve(name="%s_%s"%(hMaterialName,strainrate), xyData=xy1)
			chart.setValues(curvesToPlot=(c1, ), appendMode=True )
	
	if ((hConsiderStrainRates != True) and (hConsiderTemperatures == True)):
		myTemperaturesNumber = len(hTemperatures)
		for temp in range(0,myTemperaturesNumber,1):
			mytemp = hTemperatures[temp][0]
			list = [x[:-1] for x in myMaterialData if x[2] == mytemp]
			#print mytemp
			#print list[5]
			xy1 = xyPlot.XYData(name="%s_%s"%(hMaterialName,mytemp), data=(list), 
				sourceDescription='Entered from keyboard', axis1QuantityType=xQuantity, 
				axis2QuantityType=yQuantity, )
			c1 = session.Curve(name="%s_%s"%(hMaterialName,mytemp), xyData=xy1)
			chart.setValues(curvesToPlot=(c1, ), appendMode=True )

	if ((hConsiderStrainRates == True) and (hConsiderTemperatures == True)):
		myTemperaturesNumber = len(hTemperatures)
		myStrainratesNumber = len(hStrainrates)
		for temp in range(0,myTemperaturesNumber,1):
			mytemp = hTemperatures[temp][0]
			list = [x[:-1] for x in myMaterialData if x[3] == mytemp]
			#print list
			#print list[5]
			print ' '
			for rate in range(0,myStrainratesNumber,1):
				strainrate = hStrainrates[rate][0]
				list2 = [x[:-1] for x in list if x[2] == strainrate]
				#print list2
				#print list2[5]
				xy1 = xyPlot.XYData(name="%s_%s_%s"%(hMaterialName,strainrate,mytemp), data=(list2), 
					sourceDescription='Entered from keyboard', axis1QuantityType=xQuantity, 
					axis2QuantityType=yQuantity, )
				c1 = session.Curve(name="%s_%s_%s"%(hMaterialName,strainrate,mytemp), xyData=xy1)
				chart.setValues(curvesToPlot=(c1, ), appendMode=True )
			
	session.viewports['Viewport: 1'].setValues(displayedObject=xyp)
	elapsedTime(starttime)

def exportMaterial(hOwnExcelInterface,hPathExcelUtilities,hPathExcelExports,hMaterialName,hHSA,myMaterialData,hConsiderTemperatures,hConsiderStrainRates):
	starttime = datetime.now()
	print '... exporting material data ...'
		
	# Checking Excel-Interface
	if hOwnExcelInterface == 'Use own Excel-Interface (recommended)':
		print '... ... using own Excel-Interface'
		
		import win32com.client
		from win32com.client import constants as c
		from win32com.client import Dispatch
		import os
		
		try:
			currDir = os.path.dirname(os.path.abspath(__file__))
			myExcelExportFile = "%s/%s/%s-%s.xlsx"%(currDir,hPathExcelExports,hMaterialName,hHSA)
			
			# if os.path.exists(myExcelExportFile):
				# print '... ... ... finding existing xls-file ...'
				# f = file(myExcelExportFile, "r+")
			# else:
				# print '... ... ... file not found ...'
				# print '... ... ... creating xls-file ...'
				# f = file(myExcelExportFile, "w")
			#print '... ... accessing xls-file.'
			
			print '... ... connecting to Excel...'
			xlApp = Dispatch('Excel.Application')
			#xlApp.Workbooks.Open(myExcelExportFile)
			xlApp.Workbooks.Add()
			row = 1
			activeSheet = xlApp.ActiveSheet
			
			print '... ... exporting data ...'
			activeSheet.Cells(row,1).Value = 'Strain_phi'
			activeSheet.Cells(row,2).Value = 'Stress_sigma'
			activeSheet.Cells(row,3).Value = 'Strainrate'
			activeSheet.Cells(row,4).Value = 'Temperature'
				
			for index, data in enumerate(myMaterialData):
				#print data
				row += 1
				activeSheet.Cells(row,1).Value = data[0]
				activeSheet.Cells(row,2).Value = data[1]
				
				if (hConsiderStrainRates == True) and (hConsiderTemperatures != True):
					activeSheet.Cells(row,3).Value = data[2]
				
				if (hConsiderStrainRates != True) and (hConsiderTemperatures == True):
					activeSheet.Cells(row,4).Value = data[2]
				
				if (hConsiderStrainRates == True) and (hConsiderTemperatures == True):
					activeSheet.Cells(row,3).Value = data[2]
					activeSheet.Cells(row,4).Value = data[3]
		except:
			xlApp.DisplayAlerts = False
			xlApp.Quit()
			xlApp = None
			del xlApp
			print '****ERROR**** connecting with Excel or Excel-file'
			return
		print '... ... saving and closing Excel-file ...'
		try:
			xlApp.ActiveWorkbook.SaveAs(myExcelExportFile)
			xlApp.ActiveWorkbook.Close(SaveChanges=True)
			print "... Excel-file sucessfully created: %s" %(myExcelExportFile)
		except:
			print '****ERROR**** saving Excel-file'
			print ' '
		
		print '... closing connection to Excel ...'
		xlApp.Quit()
		del activeSheet
		xlApp = None
		del xlApp
		print '... done.'
		print ' '
		
	else:
		print ' ... ... using own Abaqus Excel-Utilities'
		import sys
		#print hPathExcelUtilities
		#print 'C:\\Program Files\\abaqus\\v6.12\\6.12-1\\code\\python\\lib\\abaqus_plugins\\excelUtilities'
		sys.path.insert(3,hPathExcelUtilities) 
		import abq_ExcelUtilities.excelUtilities
		
		try:
			plotNames = session.xyDataObjects
			for key in plotNames.keys():
				#print "key: %s , value: %s" % (key, plotNames[key])
				abq_ExcelUtilities.excelUtilities.XYtoExcel(xyDataNames="%s"%(key),trueName='From Current XY Plot')
				print " ... ... ... exported %s" % (key)
				
		except :
			print '****ERROR**** Either path to Abaqus Excel-Utilities is wrong or no XY-Data found. I this case, ensure that Plot-Material-Option is activated.'
			exit()
			return
		print '... ... done.'
	# Print computation time for this operation
	elapsedTime(starttime)

# This function ist called by the Gui
def CMTKernelFnc(
	hCreateMaterial, hPlotMaterial, hExportMaterial, hSubroutines,
	hModelName, hMaterialName, hMaterialDescription, hImport, hImportFileName,
	hKIbfC, hM1IbfC, hM2IbfC, hM3IbfC, hM4IbfC, 
	hASmC, hM1SmC, hM2SmC, hM3SmC, hM4SmC,
	hASmH, hM1SmH, hM2SmH, hM4SmH, hM5SmH, hM7SmH, hM8SmH,
	hMaximumStrain, hStrainStepSize, hConsiderTemperatures, hConsiderStrainRates, hTemperatures, hStrainrates, hHSA,
	hPathExcelUtilities, hOwnExcelInterface, hPathExcelExports):

	# Print Description of the Procedure 
	print ' '
	print '######################################################'
	print '#### CMT.py: Converts Hensel-Spittel to tabular data'
	print '######################################################'
	print ' '
	globalstarttime = datetime.now()

	if hImport == True:
		#print ">>> Checking Import-file ..."
		importedParameters = importMaterialParameters(hImportFileName, hHSA)
		
		# overwrite manuelly provided data in the Gui
		hMaterialName = str(importedParameters['hMaterialName'])
		hMaterialDescription = importedParameters['hMaterialDescription']
		hHSA = importedParameters['hHSA']
		
		if (hHSA == 'IBF(Cold)'):
			hKIbfC = importedParameters['hKIbfC']
			hM1IbfC = importedParameters['hM1IbfC']
			hM2IbfC = importedParameters['hM2IbfC']
			hM3IbfC = importedParameters['hM3IbfC']
			hM4IbfC = importedParameters['hM4IbfC']
		
		if (hHSA == 'Springer-Materials(Cold)'):
			hASmC = importedParameters['hASmC']
			hM1SmC = importedParameters['hM1SmC']
			hM2SmC = importedParameters['hM2SmC']
			hM3SmC = importedParameters['hM3SmC']
			hM4SmC = importedParameters['hM4SmC']
			
		if (hHSA == 'Springer-Materials(Hot)'):
			hASmH = importedParameters['hASmH']
			hM1SmH = importedParameters['hM1SmH']
			hM2SmH = importedParameters['hM2SmH']
			hM4SmH = importedParameters['hM4SmH']
			hM5SmH = importedParameters['hM5SmH']
			hM7SmH = importedParameters['hM7SmH']
			hM8SmH = importedParameters['hM8SmH']
		
		print '... done.'
			
	if hCreateMaterial == True:
		print ">>> Looking for %s ..." % hModelName
		while True:
			try:
				myModel = mdb.models[str(hModelName)]
				break
			except:
				print "... Not found! Creating %s ..." % hModelName
				mdb.Model(name=str(hModelName))
				myModel = mdb.models[str(hModelName)]
		print '... done.'
		print ' '

		print ">>> Creating Material: %s ..." % hMaterialName
		myMaterial = myModel.Material(name=str(hMaterialName), description=str(hMaterialDescription))
		print '... done.'
		print ' '
	
	## Check for Equation
	print ">>> Checking Hensel-Spittel-Approach ..."
	if not hHSA:
		print '****ERROR**** ... No Approach defined. Please activate either IBF- or one Springer-Materials-Approach'
		exit()
		
	elif hHSA == 'IBF(Cold)':
		print '... Identified IBF(Cold)'
		print ' '
		## IBF COLD
		if (hConsiderStrainRates != True) and (hConsiderTemperatures != True):
			# CASE 1 temperature- and strainrate-INdependent Data
			print '>>> Case 1: Calculate temperature- and strainrate-INdependent Data'
			print "... for Hensel-Spittel-Approach: %s" % hHSA
						
			# Hilfstring zum Befüllen der Tabelle. Sehr wichtig
			string = ""
			strainrate = 1
			theta = 20
			myMaterialData = []
			starttime = datetime.now()
			print '... calculate values ...'
			for strain in frange(0.05,hMaximumStrain,hStrainStepSize):
				myYieldCurve = hKIbfC*(strainrate**hM1IbfC)*(e**(theta*hM2IbfC))*(strain**hM3IbfC)*(e**(hM4IbfC/strain))
				if strain == 0.05:
					myInitialYieldStress = int(round(floor(myYieldCurve)))
					myMaterialData.append((0,myInitialYieldStress))
				string += "(%s,%s)," % (myYieldCurve,strain)		
				myMaterialData.append((strain,myYieldCurve))
			# Print computation time for this operation
			elapsedTime(starttime)	
			
			# no need to create a material if user only wants to plot or export material data
			if hCreateMaterial == True:
				starttime = datetime.now()
				print '... processing material data ...'
				# in case that the user wants to use subroutines, this if-statements creates a user defined material conatining the hensel-spittel-parameters only
				if (hSubroutines == True):
					print '... ... as user defined material for the use with UHARD/VUHARD subroutines '
					userMaterialData = "((%s, ),(%s, ),(%s, ),(%s, ),(%s, ))" % (hKIbfC,hM1IbfC,hM2IbfC,hM3IbfC,hM4IbfC)
					exec "myMaterial.Plastic(hardening=USER, table=(%s))" % userMaterialData
				else:
					print '... ... as tabular data for direct use with ABQ/CAE'
					exec "myMaterial.Plastic(table=((myInitialYieldStress,0),%s))" % string
				# Print computation time for this operation
				elapsedTime(starttime)	
			
			# the manually added or imopted data can be plotted in abq/viewer
			if hPlotMaterial == True:
				plotMaterial(hMaterialName,hHSA,myMaterialData,hConsiderTemperatures,hConsiderStrainRates,hTemperatures,hStrainrates)
			
			# for advanced data processing for presentations the tabular material data can be exportet to excel
			# in two ways: 1) using a powerful excel interface 2) using abq-excel-utilities
			if hExportMaterial == True:
				exportMaterial(hOwnExcelInterface,hPathExcelUtilities,hPathExcelExports,hMaterialName,hHSA,myMaterialData,hConsiderTemperatures,hConsiderStrainRates)

		elif (hConsiderStrainRates == True) and (hConsiderTemperatures != True):
			# CASE 2 temperature-Independet and strainrate-dependent Data
			print '>>> Case 2: Calculate strainrate-dependent Data'
			print "... for Hensel-Spittel-Approach: %s" % hHSA
			starttime = datetime.now()
			
			# Hilfstring zum Befüllen der Tabelle. Sehr wichtig
			string = ""
			theta = 20
			myMaterialData = []
			myStrainratesNumber = len(hStrainrates)
			for rate in range(0,myStrainratesNumber,1):
				strainrate = hStrainrates[rate][0]
				print "... calculate values for strainrate %s" % strainrate
				for strain in frange(0.05,hMaximumStrain,hStrainStepSize):
					myYieldCurve = hKIbfC*(strainrate**hM1IbfC)*(e**(theta*hM2IbfC))*(strain**hM3IbfC)*(e**(hM4IbfC/strain))
					if strain == 0.05:
						if rate == 0: 
							myInitialYieldStress = int(round(floor(myYieldCurve)))
							myMaterialData.append((0,myInitialYieldStress,0))
						myYieldStress = int(round(floor(myYieldCurve)))
						string += "(%s,%s,%s)," % (myYieldStress,0,strainrate)
						myMaterialData.append((0,myYieldStress,strainrate))
					string += "(%s,%s,%s)," % (myYieldCurve,strain,strainrate)
					myMaterialData.append((strain,myYieldCurve,strainrate))
			
			if hCreateMaterial == True:
				starttime = datetime.now()
				print '... processing material data ...'
				if (hSubroutines == True):
					print '... ... as user defined material for the use with UHARD/VUHARD subroutines '
					userMaterialData = "((%s, ),(%s, ),(%s, ),(%s, ),(%s, ))" % (hKIbfC,hM1IbfC,hM2IbfC,hM3IbfC,hM4IbfC)
					exec "myMaterial.Plastic(hardening=USER, table=(%s))" % userMaterialData
				else:
					print '... ... as tabular data for direct use with ABQ/CAE'
					exec "myMaterial.Plastic(rate=ON,table=((myInitialYieldStress,0,0),%s))" % string
				# Print computation time for this operation
				elapsedTime(starttime)
					
			if hPlotMaterial == True:
				plotMaterial(hMaterialName,hHSA,myMaterialData,hConsiderTemperatures,hConsiderStrainRates,hTemperatures,hStrainrates)
				
			if hExportMaterial == True:
				exportMaterial(hOwnExcelInterface,hPathExcelUtilities,hPathExcelExports,hMaterialName,hHSA,myMaterialData,hConsiderTemperatures,hConsiderStrainRates)
			
			# Print computation time for this operation
			elapsedTime(starttime)		

		elif (hConsiderTemperatures == True) and (hConsiderStrainRates != True):
			# CASE 3 temperature-dependet and strainrate-Independent Data
			print '>>> Case 3: Calculate temperature-dependent Data'
			print "... for Hensel-Spittel-Approach: %s" % hHSA
			starttime = datetime.now()
			
			# Hilfstring zum Befüllen der Tabelle. Sehr wichtig
			string = ""
			strainrate = 1
			myTempNumber = len(hTemperatures)
			myMaterialData = []
			for temp in range(0,myTempNumber,1):
				theta = hTemperatures[temp][0]
				print "... calculate values for temperature %s" % theta
				for strain in frange(0.05,hMaximumStrain,hStrainStepSize):
					myYieldCurve = hKIbfC*(strainrate**hM1IbfC)*(e**(theta*hM2IbfC))*(strain**hM3IbfC)*(e**(hM4IbfC/strain))
					if strain == 0.05:
						myInitialYieldStress = int(round(floor(myYieldCurve)))
						myMaterialData.append((0,myInitialYieldStress,theta))
					string += "(%s,%s,%s)," % (myYieldCurve,strain,theta)
					myMaterialData.append((strain,myYieldCurve,theta))
			
			if hCreateMaterial == True:
				starttime = datetime.now()
				print '... processing material data ...'
				if (hSubroutines == True):
					print '... ... as user defined material for the use with UHARD/VUHARD subroutines '
					userMaterialData = "((%s, ),(%s, ),(%s, ),(%s, ),(%s, ))" % (hKIbfC,hM1IbfC,hM2IbfC,hM3IbfC,hM4IbfC)
					exec "myMaterial.Plastic(hardening=USER, table=(%s))" % userMaterialData
				else:
					print '... ... as tabular data for direct use with ABQ/CAE'
					exec "myMaterial.Plastic(temperatureDependency=ON,table=((myInitialYieldStress,0,hTemperatures[0][0]),%s))" % string
				# Print computation time for this operation
				elapsedTime(starttime)

			if hPlotMaterial == True:
				plotMaterial(hMaterialName,hHSA,myMaterialData,hConsiderTemperatures,hConsiderStrainRates,hTemperatures,hStrainrates)
				
			if hExportMaterial == True:
				exportMaterial(hOwnExcelInterface,hPathExcelUtilities,hPathExcelExports,hMaterialName,hHSA,myMaterialData,hConsiderTemperatures,hConsiderStrainRates)
			
			# Print computation time for this operation
			elapsedTime(starttime)		
		
		elif (hConsiderTemperatures == True) and (hConsiderStrainRates == True):
			# CASE 4 temperature-dependet and strainrate-dependent Data
			print '>>> Case 4: Calculate temperature-dependent and strainrate-dependent Data'
			print "... for Hensel-Spittel-Approach: %s" % hHSA
			starttime = datetime.now()
		
			# Hilfstring zum Befüllen der Tabelle. Sehr wichtig
			string = ""
			myTempNumber = len(hTemperatures)
			myMaterialData = []
			for temp in range(0,myTempNumber,1):
				theta = hTemperatures[temp][0]
				print "... calculate values for temperature %s" % theta
				myStrainratesNumber = len(hStrainrates)
				for rate in range(0,myStrainratesNumber,1):
					strainrate = hStrainrates[rate][0]
					print "... ... calculate values for strainrate %s" % strainrate
					for strain in frange(0.05,hMaximumStrain,hStrainStepSize):
						myYieldCurve = hKIbfC*(strainrate**hM1IbfC)*(e**(theta*hM2IbfC))*(strain**hM3IbfC)*(e**(hM4IbfC/strain))
						if strain == 0.05:
							if rate == 0: 
								myInitialYieldStress = int(round(floor(myYieldCurve)))
								myMaterialData.append((0,myInitialYieldStress,0,theta))
							myYieldStress = int(round(floor(myYieldCurve)))
							string += "(%s,%s,%s,%s)," % (myYieldStress,0,strainrate,theta)
							myMaterialData.append((0,myYieldStress,strainrate,theta))
						string += "(%s,%s,%s,%s)," % (myYieldCurve,strain,strainrate,theta)
						myMaterialData.append((strain,myYieldCurve,strainrate,theta))
		
			if hCreateMaterial == True:
				starttime = datetime.now()
				print '... processing material data ...'
				if (hSubroutines == True):
					print '... ... as user defined material for the use with UHARD/VUHARD subroutines '
					userMaterialData = "((%s, ),(%s, ),(%s, ),(%s, ),(%s, ))" % (hKIbfC,hM1IbfC,hM2IbfC,hM3IbfC,hM4IbfC)
					exec "myMaterial.Plastic(hardening=USER, table=(%s))" % userMaterialData
				else:
					print '... ... as tabular data for direct use with ABQ/CAE'
					exec "myMaterial.Plastic(temperatureDependency=ON, rate=ON, table=((myInitialYieldStress,0,0,hTemperatures[0][0]),%s))" % string
				# Print computation time for this operation
				elapsedTime(starttime)
						
			if hPlotMaterial == True:
				plotMaterial(hMaterialName,hHSA,myMaterialData,hConsiderTemperatures,hConsiderStrainRates,hTemperatures,hStrainrates)
				
			if hExportMaterial == True:
				exportMaterial(hOwnExcelInterface,hPathExcelUtilities,hPathExcelExports,hMaterialName,hHSA,myMaterialData,hConsiderTemperatures,hConsiderStrainRates)
				
			# Print computation time for this operation
			elapsedTime(starttime)	

	elif hHSA == 'Springer-Materials(Cold)':
		print '... Identified Springer-Materials(Cold)'
		print ' '
		## Springer-Materials Cold
		if (hConsiderStrainRates != True) and (hConsiderTemperatures != True):
			print ' '
			# CASE 1 temperature- and strainrate-INdependent Data
			print '>>> Case 1: Calculate temperature- and strainrate-INdependent Data'
			print "... for Hensel-Spittel-Approach: %s" % hHSA
			starttime = datetime.now()
			
			# Hilfstring zum Befüllen der Tabelle. Sehr wichtig
			string = ""
			strainrate = 1
			theta = 20
			myMaterialData = []
			print '... calculate values ...'
			for strain in frange(0.05,hMaximumStrain,hStrainStepSize):
				myYieldCurve = hASmC*(e**(theta*hM1SmC))*(strain**hM2SmC)*(strainrate**hM3SmC)*(e**(hM4SmC/strain))
				if strain == 0.05:
					myInitialYieldStress = int(round(floor(myYieldCurve)))
					myMaterialData.append((0,myInitialYieldStress))
				string += "(%s,%s)," % (myYieldCurve,strain)	
				myMaterialData.append((strain,myYieldCurve))
			
			# Print computation time for this operation
			elapsedTime(starttime)	
						
			if hCreateMaterial == True:
				starttime = datetime.now()
				print '... processing material data ...'
				if (hSubroutines == True):
					print '... ... as user defined material for the use with UHARD/VUHARD subroutines '
					userMaterialData = "((%s, ),(%s, ),(%s, ),(%s, ),(%s, ))" % (hASmC,hM1SmC,hM2SmC,hM3SmC,hM4SmC)
					exec "myMaterial.Plastic(hardening=USER, table=(%s))" % userMaterialData
				else:
					print '... ... as tabular data for direct use with ABQ/CAE'
					exec "myMaterial.Plastic(table=((myInitialYieldStress,0),%s))" % string
				# Print computation time for this operation
				elapsedTime(starttime)
						
			if hPlotMaterial == True:
				plotMaterial(hMaterialName,hHSA,myMaterialData,hConsiderTemperatures,hConsiderStrainRates,hTemperatures,hStrainrates)
				
			if hExportMaterial == True:
				exportMaterial(hOwnExcelInterface,hPathExcelUtilities,hPathExcelExports,hMaterialName,hHSA,myMaterialData,hConsiderTemperatures,hConsiderStrainRates)
				
			# Print computation time for this operation
			elapsedTime(starttime)	
			
		elif (hConsiderStrainRates == True) and (hConsiderTemperatures != True):
			# CASE 2 temperature-Independet and strainrate-dependent Data
			print '>>> Case 2: Calculate strainrate-dependent Data'
			print "... for Hensel-Spittel-Approach: %s" % hHSA
			starttime = datetime.now()
			
			# Hilfstring zum Befüllen der Tabelle. Sehr wichtig
			string = ""
			theta = 20
			myMaterialData = []
			myStrainratesNumber = len(hStrainrates)
			for rate in range(0,myStrainratesNumber,1):
				strainrate = hStrainrates[rate][0]
				print "... calculate values for strainrate %s" % strainrate
				for strain in frange(0.05,hMaximumStrain,hStrainStepSize):
					myYieldCurve = hASmC*(e**(theta*hM1SmC))*(strain**hM2SmC)*(strainrate**hM3SmC)*(e**(hM4SmC/strain))
					if strain == 0.05:
						if rate == 0: 
							myInitialYieldStress = int(round(floor(myYieldCurve)))
							myMaterialData.append((0,myInitialYieldStress,0))
						myYieldStress = int(round(floor(myYieldCurve)))
						string += "(%s,%s,%s)," % (myYieldStress,0,strainrate)
						myMaterialData.append((0,myYieldStress,strainrate))
					string += "(%s,%s,%s)," % (myYieldCurve,strain,strainrate)
					myMaterialData.append((strain,myYieldCurve,strainrate))
			
			if hCreateMaterial == True:
				starttime = datetime.now()
				print '... processing material data ...'
				if (hSubroutines == True):
					print '... ... as user defined material for the use with UHARD/VUHARD subroutines '
					userMaterialData = "((%s, ),(%s, ),(%s, ),(%s, ),(%s, ))" % (hKIbfC,hM1IbfC,hM2IbfC,hM3IbfC,hM4IbfC)
					exec "myMaterial.Plastic(hardening=USER, table=(%s))" % userMaterialData
				else:
					print '... ... as tabular data for direct use with ABQ/CAE'
					exec "myMaterial.Plastic(rate=ON,table=((myInitialYieldStress,0,0),%s))" % string
				# Print computation time for this operation
				elapsedTime(starttime)
				
			if hPlotMaterial == True:
				plotMaterial(hMaterialName,hHSA,myMaterialData,hConsiderTemperatures,hConsiderStrainRates,hTemperatures,hStrainrates)
				
			if hExportMaterial == True:
				exportMaterial(hOwnExcelInterface,hPathExcelUtilities,hPathExcelExports,hMaterialName,hHSA,myMaterialData,hConsiderTemperatures,hConsiderStrainRates)
				
			# Print computation time for this operation
			elapsedTime(starttime)		

		elif (hConsiderTemperatures == True) and (hConsiderStrainRates != True):
			# CASE 3 temperature-dependet and strainrate-Independent Data
			print '>>> Case 3: Calculate temperature-dependent Data'
			print "... for Hensel-Spittel-Approach: %s" % hHSA
			starttime = datetime.now()
			
			# Hilfstring zum Befüllen der Tabelle. Sehr wichtig
			string = ""
			strainrate = 1
			myTempNumber = len(hTemperatures)
			myMaterialData = []
			for temp in range(0,myTempNumber,1):
				theta = hTemperatures[temp][0]
				print "... calculate values for temperature %s" % theta
				for strain in frange(0.05,hMaximumStrain,hStrainStepSize):
					myYieldCurve = hASmC*(e**(theta*hM1SmC))*(strain**hM2SmC)*(strainrate**hM3SmC)*(e**(hM4SmC/strain))
					if strain == 0.05:
						myInitialYieldStress = int(round(floor(myYieldCurve)))
						myMaterialData.append((0,myInitialYieldStress,theta))
					string += "(%s,%s,%s)," % (myYieldCurve,strain,theta)
					myMaterialData.append((strain,myYieldCurve,theta))
			
			if hCreateMaterial == True:
				starttime = datetime.now()
				print '... processing material data ...'
				if (hSubroutines == True):
					print '... ... as user defined material for the use with UHARD/VUHARD subroutines '
					userMaterialData = "((%s, ),(%s, ),(%s, ),(%s, ),(%s, ))" % (hKIbfC,hM1IbfC,hM2IbfC,hM3IbfC,hM4IbfC)
					exec "myMaterial.Plastic(hardening=USER, table=(%s))" % userMaterialData
				else:
					print '... ... as tabular data for direct use with ABQ/CAE'
					exec "myMaterial.Plastic(temperatureDependency=ON,table=((myInitialYieldStress,0,hTemperatures[0][0]),%s))" % string
				# Print computation time for this operation
				elapsedTime(starttime)
				
			if hPlotMaterial == True:
				plotMaterial(hMaterialName,hHSA,myMaterialData,hConsiderTemperatures,hConsiderStrainRates,hTemperatures,hStrainrates)
				
			if hExportMaterial == True:
				exportMaterial(hOwnExcelInterface,hPathExcelUtilities,hPathExcelExports,hMaterialName,hHSA,myMaterialData,hConsiderTemperatures,hConsiderStrainRates)
				
			# Print computation time for this operation
			elapsedTime(starttime)		
		
		elif (hConsiderTemperatures == True) and (hConsiderStrainRates == True):
			# CASE 4 temperature-dependet and strainrate-dependent Data
			print '>>> Case 4: Calculate temperature-dependent and strainrate-dependent Data'
			print "... for Hensel-Spittel-Approach: %s" % hHSA
			starttime = datetime.now()
		
			# Hilfstring zum Befüllen der Tabelle. Sehr wichtig
			string = ""
			myTempNumber = len(hTemperatures)
			myMaterialData = []
			for temp in range(0,myTempNumber,1):
				theta = hTemperatures[temp][0]
				print "... calculate values for temperature %s" % theta
				myStrainratesNumber = len(hStrainrates)
				for rate in range(0,myStrainratesNumber,1):
					strainrate = hStrainrates[rate][0]
					print "... ... calculate values for strainrate %s" % strainrate
					for strain in frange(0.05,hMaximumStrain,hStrainStepSize):
						myYieldCurve = hASmC*(e**(theta*hM1SmC))*(strain**hM2SmC)*(strainrate**hM3SmC)*(e**(hM4SmC/strain))
						if strain == 0.05:
							if rate == 0: 
								myInitialYieldStress = int(round(floor(myYieldCurve)))
							myMaterialData.append((0,myInitialYieldStress,0,theta))
							myYieldStress = int(round(floor(myYieldCurve)))
							string += "(%s,%s,%s,%s)," % (myYieldStress,0,strainrate,theta)
							myMaterialData.append((0,myYieldStress,strainrate,theta))
						string += "(%s,%s,%s,%s)," % (myYieldCurve,strain,strainrate,theta)
						myMaterialData.append((strain,myYieldCurve,strainrate,theta))
		
			if hCreateMaterial == True:
				starttime = datetime.now()
				print '... processing material data ...'
				if (hSubroutines == True):
					print '... ... as user defined material for the use with UHARD/VUHARD subroutines '
					userMaterialData = "((%s, ),(%s, ),(%s, ),(%s, ),(%s, ))" % (hKIbfC,hM1IbfC,hM2IbfC,hM3IbfC,hM4IbfC)
					exec "myMaterial.Plastic(hardening=USER, table=(%s))" % userMaterialData
				else:
					print '... ... as tabular data for direct use with ABQ/CAE'
					exec "myMaterial.Plastic(temperatureDependency=ON, rate=ON, table=((myInitialYieldStress,0,0,hTemperatures[0][0]),%s))" % string
				# Print computation time for this operation
				elapsedTime(starttime)
		
			if hPlotMaterial == True:
				plotMaterial(hMaterialName,hHSA,myMaterialData,hConsiderTemperatures,hConsiderStrainRates,hTemperatures,hStrainrates)
				
			if hExportMaterial == True:
				exportMaterial(hOwnExcelInterface,hPathExcelUtilities,hPathExcelExports,hMaterialName,hHSA,myMaterialData,hConsiderTemperatures,hConsiderStrainRates)
				
			# Print computation time for this operation
			elapsedTime(starttime)		

	elif hHSA == 'Springer-Materials(Hot)':
		print '... Identified Springer-Materials(Hot)'
		print ' '
		## Springer-Materials Hot
		if (hConsiderStrainRates != True) and (hConsiderTemperatures != True):
			print ' '
			# CASE 1 temperature- and strainrate-INdependent Data
			print '>>> Case 1: Calculate temperature- and strainrate-INdependent Data'
			print "... for Hensel-Spittel-Approach: %s" % hHSA
			starttime = datetime.now()
			
			# Hilfstring zum Befüllen der Tabelle. Sehr wichtig
			string = ""
			strainrate = 10
			theta = 800
			myMaterialData = []
			print '... calculate values ...'
			for strain in frange(0.05,hMaximumStrain,hStrainStepSize):
				myYieldCurve = hASmH*(e**(theta*hM1SmH))*(strain**hM2SmH)*(e**(hM4SmH/strain))*((1+strain)**hM5SmH*theta)*(e**(hM7SmH*strain))*(strainrate**(hM8SmH*theta))*1/1000
				if strain == 0.05:
					myInitialYieldStress = int(round(floor(myYieldCurve)))
					myMaterialData.append((0,myInitialYieldStress))
				string += "(%s,%s)," % (myYieldCurve,strain)		
				myMaterialData.append((strain,myYieldCurve))
			
			# Print computation time for this operation
			elapsedTime(starttime)	
						
			if hCreateMaterial == True:
				starttime = datetime.now()
				print '... processing material data ...'
				if (hSubroutines == True):
					print '... ... as user defined material for the use with UHARD/VUHARD subroutines '
					userMaterialData = "((%s, ),(%s, ),(%s, ),(%s, ),(%s, ))" % (hKIbfC,hM1IbfC,hM2IbfC,hM3IbfC,hM4IbfC)
					exec "myMaterial.Plastic(hardening=USER, table=(%s))" % userMaterialData
				else:
					print '... ... as tabular data for direct use with ABQ/CAE'
					exec "myMaterial.Plastic(table=((myInitialYieldStress,0),%s))" % string
				# Print computation time for this operation
				elapsedTime(starttime)
					
			if hPlotMaterial == True:
				plotMaterial(hMaterialName,hHSA,myMaterialData,hConsiderTemperatures,hConsiderStrainRates,hTemperatures,hStrainrates)
				
			if hExportMaterial == True:
				exportMaterial(hOwnExcelInterface,hPathExcelUtilities,hPathExcelExports,hMaterialName,hHSA,myMaterialData,hConsiderTemperatures,hConsiderStrainRates)
				
			# Print computation time for this operation
			elapsedTime(starttime)
			
		elif (hConsiderStrainRates == True) and (hConsiderTemperatures != True):
			# CASE 2 temperature-Independet and strainrate-dependent Data
			print '>>> Case 2: Calculate strainrate-dependent Data'
			print "... for Hensel-Spittel-Approach: %s" % hHSA
			starttime = datetime.now()
			
			# Hilfstring zum Befüllen der Tabelle. Sehr wichtig
			string = ""
			theta = 20
			myMaterialData = []
			myStrainratesNumber = len(hStrainrates)
			for rate in range(0,myStrainratesNumber,1):
				strainrate = hStrainrates[rate][0]
				print "... calculate values for strainrate %s" % strainrate
				for strain in frange(0.05,hMaximumStrain,hStrainStepSize):
					myYieldCurve = hASmH*(e**(theta*hM1SmH))*(strain**hM2SmH)*(e**(hM4SmH/strain))*((1+strain)**hM5SmH*theta)*(e**(hM7SmH*strain))*(strainrate**(hM8SmH*theta))
					if strain == 0.05:
						if rate == 0: 
							myInitialYieldStress = int(round(floor(myYieldCurve)))
							myMaterialData.append((0,myInitialYieldStress,0))
						myYieldStress = int(round(floor(myYieldCurve)))
						string += "(%s,%s,%s)," % (myYieldStress,0,strainrate)
						myMaterialData.append((0,myYieldStress,strainrate))
					string += "(%s,%s,%s)," % (myYieldCurve,strain,strainrate)
					myMaterialData.append((strain,myYieldCurve,strainrate))
			
			if hCreateMaterial == True:
				starttime = datetime.now()
				print '... processing material data ...'
				if (hSubroutines == True):
					print '... ... as user defined material for the use with UHARD/VUHARD subroutines '
					userMaterialData = "((%s, ),(%s, ),(%s, ),(%s, ),(%s, ))" % (hKIbfC,hM1IbfC,hM2IbfC,hM3IbfC,hM4IbfC)
					exec "myMaterial.Plastic(hardening=USER, table=(%s))" % userMaterialData
				else:
					print '... ... as tabular data for direct use with ABQ/CAE'
					exec "myMaterial.Plastic(rate=ON,table=((myInitialYieldStress,0,0),%s))" % string
				# Print computation time for this operation
				elapsedTime(starttime)
			
			if hPlotMaterial == True:
				plotMaterial(hMaterialName,hHSA,myMaterialData,hConsiderTemperatures,hConsiderStrainRates,hTemperatures,hStrainrates)
				
			if hExportMaterial == True:
				exportMaterial(hOwnExcelInterface,hPathExcelUtilities,hPathExcelExports,hMaterialName,hHSA,myMaterialData,hConsiderTemperatures,hConsiderStrainRates)
				
			# Print computation time for this operation
			elapsedTime(starttime)	

		elif (hConsiderTemperatures == True) and (hConsiderStrainRates != True):
			# CASE 3 temperature-dependet and strainrate-Independent Data
			print '>>> Case 3: Calculate temperature-dependent Data'
			print "... for Hensel-Spittel-Approach: %s" % hHSA
			starttime = datetime.now()
			
			# Hilfstring zum Befüllen der Tabelle. Sehr wichtig
			string = ""
			strainrate = 10
			myTempNumber = len(hTemperatures)
			myMaterialData = []
			for temp in range(0,myTempNumber,1):
				theta = hTemperatures[temp][0]
				print "... calculate values for temperature %s" % theta
				for strain in frange(0.05,hMaximumStrain,hStrainStepSize):
					myYieldCurve = hASmH*(e**(theta*hM1SmH))*(strain**hM2SmH)*(e**(hM4SmH/strain))*((1+strain)**hM5SmH*theta)*(e**(hM7SmH*strain))*(strainrate**(hM8SmH*theta))
					if strain == 0.05:
						myInitialYieldStress = int(round(floor(myYieldCurve)))
						myMaterialData.append((0,myInitialYieldStress,theta))
					string += "(%s,%s,%s)," % (myYieldCurve,strain,theta)
					myMaterialData.append((strain,myYieldCurve,theta))
			
			if hCreateMaterial == True:
				starttime = datetime.now()
				print '... processing material data ...'
				if (hSubroutines == True):
					print '... ... as user defined material for the use with UHARD/VUHARD subroutines '
					userMaterialData = "((%s, ),(%s, ),(%s, ),(%s, ),(%s, ))" % (hKIbfC,hM1IbfC,hM2IbfC,hM3IbfC,hM4IbfC)
					exec "myMaterial.Plastic(hardening=USER, table=(%s))" % userMaterialData
				else:
					print '... ... as tabular data for direct use with ABQ/CAE'
					exec "myMaterial.Plastic(temperatureDependency=ON,table=((myInitialYieldStress,0,hTemperatures[0][0]),%s))" % string
				# Print computation time for this operation
				elapsedTime(starttime)
			
			if hPlotMaterial == True:
				plotMaterial(hMaterialName,hHSA,myMaterialData,hConsiderTemperatures,hConsiderStrainRates,hTemperatures,hStrainrates)
				
			if hExportMaterial == True:
				exportMaterial(hOwnExcelInterface,hPathExcelUtilities,hPathExcelExports,hMaterialName,hHSA,myMaterialData,hConsiderTemperatures,hConsiderStrainRates)
				
			# Print computation time for this operation
			elapsedTime(starttime)	
		
		elif (hConsiderTemperatures == True) and (hConsiderStrainRates == True):
			# CASE 4 temperature-dependet and strainrate-dependent Data
			print '>>> Case 4: Calculate temperature-dependent and strainrate-dependent Data'
			print "... for Hensel-Spittel-Approach: %s" % hHSA
			starttime = datetime.now()
		
			# Hilfstring zum Befüllen der Tabelle. Sehr wichtig
			string = ""
			myTempNumber = len(hTemperatures)
			myMaterialData = []
			for temp in range(0,myTempNumber,1):
				theta = hTemperatures[temp][0]
				print "... calculate values for temperature %s" % theta
				myStrainratesNumber = len(hStrainrates)
				for rate in range(0,myStrainratesNumber,1):
					strainrate = hStrainrates[rate][0]
					print "... ... calculate values for strainrate %s" % strainrate
					for strain in frange(0.05,hMaximumStrain,hStrainStepSize):
						myYieldCurve = hASmH*(e**(theta*hM1SmH))*(strain**hM2SmH)*(e**(hM4SmH/strain))*((1+strain)**hM5SmH*theta)*(e**(hM7SmH*strain))*(strainrate**(hM8SmH*theta))
						if strain == 0.05:
							if rate == 0: 
								myInitialYieldStress = int(round(floor(myYieldCurve)))
								myMaterialData.append((0,myInitialYieldStress,0,theta))
							myYieldStress = int(round(floor(myYieldCurve)))
							string += "(%s,%s,%s,%s)," % (myYieldStress,0,strainrate,theta)
							myMaterialData.append((0,myYieldStress,strainrate,theta))
						string += "(%s,%s,%s,%s)," % (myYieldCurve,strain,strainrate,theta)
						myMaterialData.append((strain,myYieldCurve,strainrate,theta))
		
			if hCreateMaterial == True:
				starttime = datetime.now()
				print '... processing material data ...'
				if (hSubroutines == True):
					print '... ... as user defined material for the use with UHARD/VUHARD subroutines '
					userMaterialData = "((%s, ),(%s, ),(%s, ),(%s, ),(%s, ))" % (hKIbfC,hM1IbfC,hM2IbfC,hM3IbfC,hM4IbfC)
					exec "myMaterial.Plastic(hardening=USER, table=(%s))" % userMaterialData
				else:
					print '... ... as tabular data for direct use with ABQ/CAE'
					exec "myMaterial.Plastic(temperatureDependency=ON, rate=ON, table=((myInitialYieldStress,0,0,hTemperatures[0][0]),%s))" % string
				# Print computation time for this operation
				elapsedTime(starttime)
			
			if hPlotMaterial == True:
				plotMaterial(hMaterialName,hHSA,myMaterialData,hConsiderTemperatures,hConsiderStrainRates,hTemperatures,hStrainrates)
				
			if hExportMaterial == True:
				exportMaterial(hOwnExcelInterface,hPathExcelUtilities,hPathExcelExports,hMaterialName,hHSA,myMaterialData,hConsiderTemperatures,hConsiderStrainRates)
				
			# Print computation time for this operation
			elapsedTime(starttime)	

	endtime = datetime.now()
	dauer = endtime - globalstarttime
	print ' '
	print '####'
	print "#### Completed succesfully in h:m:s:ms = %s" % (dauer)
	print '####'
	print '#######################################################'

	#session.viewports['Viewport: 1'].setValues(displayedObject=p)
	#session.viewports['Viewport: 1'].forceRefresh()
	
	# funktionierte:
	#K = 790
	#strainrate = 1
	#theta = 293
	#m1 = 0.0108123
	#m2 = -0.00051014
	#m3 = 0.14494796
	#m4 = -0.00577363
	#strainmaximum = 4
	#strainstepsize = 0.05
	#string = ""
	#myMaterial = mdb.models['Model-1'].Material(name='Hensel-Spittel-2')
	#for strain in frange(0.05,strainmaximum,strainstepsize):
	#	myYieldCurve = K*(strainrate**m1)*(e**(theta*m2))*(strain**m3)*(e**(m4/strain))
	#	if strain == 0.05:
	#		myInitialYieldStress = floor(myYieldCurve)
	#	string += "(%s,%s)," % (myYieldCurve,strain)
#
	#exec "myMaterial.Plastic(table=((myInitialYieldStress,0),%s))" % string # rate=ON temperatureDependency=ON