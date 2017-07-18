# USR_VFriction

Description of the content of the directories:

GUI

	Simple Wizard (Abaqus plug-in) for configuring an Abaqus model for the USR VFRICTION

Kontinuum

	VFRICTION_Conti.for
		User Subroutine for a Continuum model
		
	Job-Kontinuum-raw.inp
		.inp-File before the wizard
		
	Job-Kontinuum.inp
		Final .inp-File after the wizard
Shell

	VFRICTION_Shell.for
		User Subroutine for a shell model (the variables firstIntProp and secondIntProp have to be changed by hand)
		
	Job-Shell-raw.inp
		.inp-File before the wizard
		
	Job-Shell.inp
		Final .inp-File after the wizard

Tool

	vfriction-2lplot.for
		User SUbroutine for tools
		
	Job-Tool-raw.inp
		.inp-File before the wizard
	
	Job-Tool.inp
		Final .inp-File after the wizard



How to include the User Subroutine using the Abaqus plugin


1. Create .inp-File of the Abaqus model
2. Open the GUI plug-in in Abaqus and follow the menue
3. Compile the corresponding Fortran Subroutine (Continuum or Shell) in your Working Directory
4. Start the .inp-File in your Working Directory (with compiled USR, .env-File and empty "TMP_Scratch"-Directory) via Batch-File or command window
5. The results can be seen in the Abaqus-Viewer as FV (for Tools Result->Options unselect "Average element output at nodes")





Beschreibung der Inhalte der einzelnen Ordner:

GUI

	Einfaches Abaqus-Plugin zur Konfiguration eines Modells für die USR VFRICTION

Kontinuum

	VFRICTION_Conti.for
	
		User Subroutine fuer Kontinuum-Modelle
		
	Job-Kontinuum-raw.inp
	
		.inp-File vor Anwendung der GUI
		
	Job-Kontinuum.inp
	
		Finales .inp-File nach Anwendung der GUI
		

Shell

	VFRICTION_Shell.for
	
		User Subroutine fuer Shell-Modelle (Variablen firstIntProp und secondIntProp muessen von Hand angepasst werden)
		
	Job-Shell-raw.inp
	
		.inp-File vor Anwendung der GUI
		
	Job-Shell.inp
	
		Finales .inp-File nach Anwendung der GUI
		

Shell

	vfriction-2lplot.for
	
		User Subroutine fuer Werkzeuge
		
	Job-Tool-raw.inp
	
		.inp-File vor Anwendung der GUI
		
	Job-Tool.inp
	
		Finales .inp-File nach Anwendung der GUI


Anleitung zur Einbindung der User-Subroutine mit Hilfe des Plugins

1. .inp-File des Abaqus-Modells erstellen
2. GUI-Plugin in Abaqus öffnen und Menue befolgen
3. Entsprechende Fortran-Subroutine (Kontinuum oder Shell) im Arbeitsordner kompilieren
4. .inp-File im Arbeitsorder (mit kompilierter USR, .env-Datei und leerem "TMP_Scratch"-Ordner) per Batchdatei (oder command window) starten
5. Ergebnisse sind zu sehen im Abaqus-Viewer unter FV (bei Werkzeug Result->Options Haken entfernen bei "Average element output at nodes")
