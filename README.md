# USR_VFriction
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
	
		User Subroutine fuer Shell-Modelle (Variablen firstIntProp und seconIntProp muessen von Hand angepasst werden)
		
	Job-Shell-raw.inp
	
		.inp-File vor Anwendung der GUI
		
	Job-Shell.inp
	
		Finales .inp-File nach Anwendung der GUI


Anleitung zur Einbindung der User-Subroutine mit Hilfe des Plugins

1. .inp-File des Abaqus-Modells erstellen
2. GUI-Plugin in Abaqus öffnen und Menue befolgen
3. Entsprechende Fortran-Subroutine (Kontinuum oder Shell) im Arbeitsordner kompilieren
4. .inp-File im Arbeitsorder (mit kompilierter USR, .env-Datei und leerem "TMP_Scratch"-Ordner) per Batchdatei (oder command window) starten
5. Ergebnisse sind zu sehen im Abaqus-Viewer unter FV
