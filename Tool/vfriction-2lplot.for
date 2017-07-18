       subroutine vfriction (
C Write only - 
     *   fTangential, 
C Read/Write - 
     *   state,
C Read only - 
     *   nBlock, nBlockAnal, nBlockEdge, 
     *   nNodState, nNodSlv, nNodMst, 
     *   nFricDir, nDir, 
     *   nStates, nProps, nTemp, nFields, 
     *   jFlags, rData, 
     *   surfInt, surfSlv, surfMst, 
     *   jConSlvUid, jConMstUid, props, 
     *   dSlipFric, fStickForce, fTangPrev, fNormal, 
     *   areaCont, dircosN, dircosS1, 
     *   shapeSlv, shapeMst, 
     *   coordSlv, coordMst, 
     *   velSlv, velMst, 
     *   tempSlv, tempMst, 
     *   fieldSlv, fieldMst )
C
      include 'vaba_param.inc'
C
      dimension fTangential(nFricDir,nBlock),
     *   state(nStates,nNodState,nBlock),
     *   jConSlvUid(nNodSlv,nBlock), 
     *   jConMstUid(nNodMst,nBlockAnal), 
     *   props(nProps),
     *   dSlipFric(nDir,nBlock),
     *   fStickForce(nBlock), 
     *   fTangPrev(nDir,nBlock),
     *   fNormal(nBlock), 
     *   areaCont(nBlock),
     *   dircosN(nDir,nBlock),
     *   dircosS1(nDir,nBlock), 
     *   shapeSlv(nNodSlv,nBlockEdge), 
     *   shapeMst(nNodMst,nBlockAnal), 
     *   coordSlv(nDir,nNodSlv,nBlock), 
     *   coordMst(nDir,nNodMst,nBlockAnal), 
     *   velSlv(nDir,nNodSlv,nBlock), 
     *   velMst(nDir,nNodMst,nBlockAnal), 
     *   tempSlv(nBlock), 
     *   tempMst(nBlockAnal),
     *   fieldSlv(nFields,nBlock),
     *   fieldMst(nFields,nBlockAnal)
C
      parameter( iKStep    = 1,
     *           iKInc     = 2,
     *           iLConType = 3,
     *           nFlags    = 3 )
C
      parameter( iTimStep      = 1,
     *           iTimGlb       = 2,
     *           iDTimCur      = 3,
     *           iFrictionWork = 4, 
     *           nData         = 4 )
C
      dimension jFlags(nFlags), rData(nData)
C
      character*80 surfInt, surfSlv, surfMst 
C
ccc-----------------------------------------------------------------ccc
ccc   Benutzer Code zur Definition der VFRICTION Subroutine         ccc
ccc-----------------------------------------------------------------ccc
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccc                \/  Variablen Deklarationen \/                   ccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
ccc   ProbSize bestimmt die Groeße der common Blocks. 
ccc   ProbSize muss mindestens der Anz. der Knoten im zu untersuchenden
ccc   Node Set sein.
c 
      integer ProbSizeZR
c
      parameter (ProbSizeZR=400000)
c
ccc   SMAAspUserSubroutines wird eingefügt, um die get_thread_id() 
ccc   nutzen zu können. myThrID wird nach Aufrufen von get_thread_id()
ccc   die ID des aktiven Threads beinhalten.
c
#include <SMAAspUserSubroutines.hdr>
c
      integer myThrID
c
ccc   numThreads wird nach Aufrufen von getNumThreads() auf die Anzahl
ccc   der zur parallelisierung verwendeten Threads gesetzt.
c 
      integer numThreads
c
ccc   NumOfCPU wird auf die Anzahl der verwendeten CPUs gesetzt und 
ccc   wird zu Dimensionierungszwecken gebraucht.
c
      integer NumOfCPU
      parameter (NumOfCPU=22)
c
ccc   ContMuZR ist das Array zur Uebergabe der Reibungskoeffizienten
ccc   am Ziehring aus der VFRICTION in die VUFIELD USR.
c
      real ContMuZR(ProbSizeZR,NumOfCPU)
c
ccc   CallCount zählt, wie oft in diesem Inkrement ein Masterknoten
ccc   bearbeitet wurde
c
      Integer CallCountZR(ProbSizeZR,NumOfCPU)
c
ccc   WeightedMu dient als Array zur Berechnung eines Durchschnitts-
ccc   wertes der Reibungskoeffizienten
c
c      double precision WeightedMu(ProbSize)
c
ccc   Zur Berechnung des Mittelwertes wird die Summe ueber die Zeit-
ccc   inkremente genutzt, bei der ein Reibungskoeffizient ungleich
ccc   0 berechnet wurde
c
c      real TimeSum(ProbSize)
c
ccc   Einigabe Variablen, koeffizienten des Reibgesetzes 
c
      real c, p_exp, v_exp, t_exp, m_exp
c
ccc   Einige physikalische Variablen. p=Druck, 
ccc   v=Relativgeschwindigkeit, t=Temperatur, ft=Tangentialkraft
ccc   disp=Verschiebung, m=Schmierstoffdicke
c
      real p, v, t, m, ft, disp
c      real press, ve, temp, ft, disp
c      
ccc   Kritische Werte des Drukts p, Geschwindigkeit v, temperatur t
c
      real p_crit, v_crit, t_crit
      parameter (p_crit=1.0, v_crit=10.0, t_crit=20.0) 
c
ccc   Temporäre Variable für den Reibkoeffizienten
c 
      real muTemp
c
ccc   CNI und CNS zeigen von welchem Inkrement und
ccc   Schritt die Reibungskoeffizienten in ContMu stammen.
ccc   CurrentInc und CurrentStep beinhalten die aktuelle Inkrement Nr.
ccc   und Step Nr..
c
      integer CNI_ZR(ProbSizeZR,NumOfCPU), 
     *        CNS_ZR(ProbSizeZR,NumOfCPU)
c
      integer CurrentInc, CurrentStep
c
ccc   MuSum Speichert die ueber die Simulation aufsummierten 
ccc   Reibkoeffizienten
c 
      double precision MuSum(ProbSizeZR,NumOfCPU)
c
ccc   MuSumCallCunt speichert die ueber die Simulation insgesammt
ccc   berechneten Reibkoeffizienten
c 
      integer MuSumCallCunt(ProbSizeZR,NumOfCPU)
c
ccc   Die ContMu und CallCount MuSum und MuSumCallCunt Arrays werden 
ccc   als common Blocks definiert
c
      common /KontaktReal/ ContMuZR, CallCountZR, MuSum, MuSumCallCunt
c
ccc  Die Variablen CurrentInc und CurrentStep sowie die 
ccc  Knotenspezifischen Arrays CNI und CNS werden als 
ccc  common Blocks definiert.
c
      common /KTime/ CurrentInc, CurrentStep, CNI_ZR, CNS_ZR
c
ccc save befehl stellt sicher, dass die common Blocks nach verlassen
ccc der Subroutine erhalten bleibt.
c
      save /KontaktReal/
      save /KTime/
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccc           /\ Ende der Variablen Deklarationen /\                ccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c
ccc   Die aktuelle Inkrement Nr. und Schritt Nr. wird Upgedatet
c
      CurrentInc=jFlags(2)
      CurrentStep=jFlags(1)
c
ccc   myThrID wird auf die aktive Thread Nr. gesetzt.
c	  
      myThrID = get_thread_id() +1
c
ccc   numThreads wird auf die für die Parallelisierung verwendete Anzahl
ccc   von Threads gesetzt.
c
      numThreads = getNumThreads()
c
ccc   Wenn die Variable NumOfCPU nicht der Anzahl an Threads entspricht,
ccc   wird eine Warnung in der .log File ausgegeben.
c
      if (numThreads .gt. NumOfCPU) then
         print *, '***'
         print *, 'ACHTUNG! Die Variable NumOfCPU entspricht nicht der'
         print *, 'Anzahl der Threads. Bitte im Code anpassen!'
         print *, '***'
      end if
c
ccc   Die gewaehlte Berechnungsvorschrift für die 
ccc   Reibungskoeffizientenwird werden aus der Abaqus Inputfile an
ccc   Abh in Form eines numerischen Wertes uebergeben.
c
ccc-----------------------------------------------------------------ccc
ccc   Reibungskoeffizient wird druck-, rel. geschwindigkeits- und   ccc
ccc   temperaturabhaengig berechnet.                                ccc
ccc-----------------------------------------------------------------ccc
c
ccc   Übergabe der Eingabe Daten aus der Input File an lokale Variablen
c
      c  =  props(1)
      p_exp=props(2)
      v_exp=props(3)
      t_exp=props(4)
      m_exp=props(5) 
c
ccc   Dummy Schmierstoffmenge
c
      m=1.0
c
ccc   Schleife über alle Knoten zur berechnung der Reibkoeffizienten
c
      do k = 1, nBlock
c
ccc   Berechnung der Verschiebung
c
       disp=sqrt(dSlipFric(1,k)**2.0 +
     *           dSlipFric(2,k)**2.0 +
     *           dSlipFric(3,k)**2.0)
c
ccc   if-Abfrage um Divisionen durch 0 zu vermeiden
c
       if (fNormal(k)      .ne. 0.0 .and.
     *     rData(iDTimCur) .ne. 0.0 .and.
     *     disp            .ne. 0.0) then
c
ccc   Berechnung des lokalen Kontaktdrucks zwischen Slaveknoten und
ccc   Masterfläche
c
        p=fNormal(k)/areaCont(k)
c
ccc   Berechnung der Relativgeschwindigkeit des aktuellen Slaveknotens
c
        v=disp/rData(iDTimCur)
c
ccc   Berechnung der lokalen Temperatur als Mittelwert zwischen
ccc   Slaveknoten und Masterfläche
c
        t=(tempMst(k)+tempSlv(k))/2.0
c
ccc     Liegt der Druck, die Temperatur oder die Geschwindigkeit unter
ccc     einer Grenze, wird die Variable auf einen Minimalwert gesetzt
c
        if (p.LT.p_crit) p=p_crit
        if (v.LT.v_crit) v=v_crit
        if (t.LT.t_crit) t=t_crit
c
ccc   Berechnung des lokalen Reibkoeffizienten nach Filzeks Reibmodell
ccc   Version MA trt_eb
c
        muTemp=c * p**p_exp * v**v_exp * t**t_exp * m**m_exp
c
ccc   Bedingung zur Erhaltung der Haftbedingung. Die Tangentialkraft
ccc   darf nicht größer sein, als die von Abaqus bestimmte fStickForce
ccc   (siehe Abaqus Manual)
c
        ft = min ( muTemp*fNormal(k), 
     *             fStickForce(k) )
c
ccc   fTangential die Abaqus Variable zur Bestimmung der Reibkraft.
ccc   Die erste Komponente ist in Bewegungsrichtung, die zweite orthogonal
ccc   zu dieser und wird deswegen auf 0 gesetzt.
c
        fTangential(1,k) = -ft
        fTangential(2,k) = 0.0
c
ccc   Durch die folgende if-Verzweigung werden der Kontakt zwischen,
ccc   
c
        if (surfInt .eq. 'CONTACT-ZR') then
c
ccc   Eine Schleife über die Masterknoten der Fläche, die mit dem 
ccc   Slaveknoten in Kontakt ist.
c
         do i=1,nNodMst
c
ccc   if-Abfrage um zu prüfen, ob der Thread den Masterknoten in diesem
ccc   Inkrement nicht zum ersten mal bearbeitet
c
          if (CNI_ZR(jConMstUid(i,k),myThrID) .eq. CurrentInc .and.
     *        CNS_ZR(jConMstUid(i,k),myThrID) .eq. CurrentStep) then
c
ccc   CallCount wird für den aktuellen Masterknoten und Thread jedes mal
ccc   um 1 erhöht, wenn der Masterknoten im gleichen Inkrement wieder
ccc   bearbeitet wird.
c 
           CallCountZR(jConMstUid(i,k),myThrID)=
     *      CallCountZR(jConMstUid(i,k),myThrID)+1
c 
ccc   In ContMu werden für jeden Thread und Masterknoten in den
ccc   Inkrementen aufadiert.
c 
           ContMuZR(jConMstUid(i,k),myThrID)=
     *      ContMuZR(jConMstUid(i,k),myThrID)+muTemp
c
ccc   Wird der Masterknoten von dem Thread in diesem Inkrement zum
ccc   ersten mal bearbeitet, wird CallCount auf 1, ContMu auf den
ccc   berechneten Reibkoeffizienten, CNI auf die aktuelle Inkrementnr.
ccc   und CNS auf die aktuelle Stepnr. gesetzt.
c
          else
           CallCountZR(jConMstUid(i,k),myThrID)=1
           ContMuZR(jConMstUid(i,k),myThrID)=muTemp
           CNI_ZR(jConMstUid(i,k),myThrID)=CurrentInc
           CNS_ZR(jConMstUid(i,k),myThrID)=CurrentStep
          end if
c
ccc   Eine Schleife über die Masterknoten der Fläche, die mit dem 
ccc   Slaveknoten in Kontakt ist.
c
         end do
         do i=1,nNodMst
c
ccc       Zu MuSum wird bei jeder weiteren Reibwertberechnung an einem
ccc       speziellen Knoten der neue hinzuadiert
c 
          MuSum(jConMstUid(i,k),myThrID)=
     *     MuSum(jConMstUid(i,k),myThrID)+muTemp
c
ccc      Die Gesamtzahl der berechneten Reibkoeffizienten an dem
ccc      aktuellen Knoten wird um 1 erhoet.
c
          MuSumCallCunt(jConMstUid(i,k),myThrID)=
     *     MuSumCallCunt(jConMstUid(i,k),myThrID)+1
    
         end do
        end if
       end if 
      end do
      return
      end
c
      SUBROUTINE VUFIELD(FIELD, NBLOCK, NFIELD, KFIELD, NCOMP,
     1                   KSTEP, JFLAGS, JNODEID, TIME,
     2                   COORDS, U, V, A)
C
      INCLUDE 'VABA_PARAM.INC'

C     indices for the time array TIME
      PARAMETER( i_ufld_Current   = 1,
     *           i_ufld_Increment = 2,
     *           i_ufld_Period    = 3,
     *           i_ufld_Total     = 4 )

C     indices for the coordinate array COORDS
      PARAMETER( i_ufld_CoordX = 1,
     *           i_ufld_CoordY = 2,
     *           i_ufld_CoordZ = 3 )

C     indices for the displacement array U
      PARAMETER( i_ufld_SpaDisplX = 1,
     *           i_ufld_SpaDisplY = 2,
     *           i_ufld_SpaDisplZ = 3,
     *           i_ufld_RotDisplX = 4,
     *           i_ufld_RotDisplY = 5,
     *           i_ufld_RotDisplZ = 6,
     *           i_ufld_AcoPress  = 7,
     *           i_ufld_Temp      = 8 )

C     indices for the velocity array V
      PARAMETER( i_ufld_SpaVelX   = 1,
     *           i_ufld_SpaVelY   = 2,
     *           i_ufld_SpaVelZ   = 3,
     *           i_ufld_RotVelX   = 4,
     *           i_ufld_RotVelY   = 5,
     *           i_ufld_RotVelZ   = 6,
     *           i_ufld_DAcoPress = 7,
     *           i_ufld_DTemp     = 8 )

C     indices for the acceleration array A
      PARAMETER( i_ufld_SpaAccelX  = 1,
     *           i_ufld_SpaAccelY  = 2,
     *           i_ufld_SpaAccelZ  = 3,
     *           i_ufld_RotAccelX  = 4,
     *           i_ufld_RotAccelY  = 5,
     *           i_ufld_RotAccelZ  = 6,
     *           i_ufld_DDAcoPress = 7,
     *           i_ufld_DDTemp     = 8 )

C     indices for JFLAGS
      PARAMETER( i_ufld_kInc   = 1,
     *           i_ufld_kPass  = 2 )
C
      DIMENSION FIELD(NBLOCK,NCOMP,NFIELD)
      DIMENSION JFLAGS(2), JNODEID(NBLOCK), TIME(4), 
     *          COORDS(3,NBLOCK)
      DIMENSION U(8,NBLOCK), V(8,NBLOCK), A(8,NBLOCK)
C
ccc-----------------------------------------------------------------ccc
ccc   Benutzer Code zur definition der VUFIELD Subroutine           ccc
ccc-----------------------------------------------------------------ccc
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccc                \/  Variablen Deklarationen \/                   ccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
ccc   ProbSize bestimmt die Groeße der common Blocks. 
ccc   ProbSize muss mindestens der Anz. der Knoten im zu untersuchenden
ccc   Node Set sein.
c 
      integer ProbSizeZR
c
      parameter (ProbSizeZR=400000)
c
ccc   SMAAspUserSubroutines wird eingefügt, um die get_thread_id() 
ccc   nutzen zu können. myThrID wird nach Aufrufen von get_thread_id()
ccc   die ID des aktiven Threads beinhalten.
c
#include <SMAAspUserSubroutines.hdr>
c
      integer myThrID
c
ccc   numThreads wird nach Aufrufen von getNumThreads() auf die Anzahl
ccc   der zur parallelisierung verwendeten Threads gesetzt.
c 
      integer numThreads
c 
ccc   tempVar ist eine temporär Variable für die Fieldvariablen 
ccc   Aktuallisierung
c 
      real tempVar
c
ccc   help ist eine Hilfvariable zum Feststellung von wie vielen CPUs
ccc   ein Knoten bearbeitet wurde.
c
      integer help
c
ccc   NumOfCPU wird auf die Anzahl der verwendeten CPUs gesetzt und 
ccc   wird zu Dimensionierungszwecken gebraucht.
c
      integer NumOfCPU
      parameter (NumOfCPU=22)
c
ccc   ContMuZR ist das Array zur Uebergabe der Reibungskoeffizienten
ccc   am Ziehring aus der VFRICTION in die VUFIELD USR.
c
      real ContMuZR(ProbSizeZR,NumOfCPU)
c
ccc   CallCount zählt, wie oft in diesem Inkrement ein Masterknoten
ccc   bearbeitet wurde
c
      Integer CallCountZR(ProbSizeZR,NumOfCPU)
c
ccc   MuSum Speichert die ueber die Simulation aufsummierten 
ccc   Reibkoeffizienten
c 
      double precision MuSum(ProbSizeZR,NumOfCPU)
c
ccc   MuSumCallCunt speichert die ueber die Simulation insgesammt
ccc   berechneten Reibkoeffizienten
c 
      integer MuSumCallCunt(ProbSizeZR,NumOfCPU)
c
ccc   CNI und CNS zeigen von welchem Inkrement und
ccc   Schritt die Reibungskoeffizienten in ContMu stammen.
ccc   CurrentInc und CurrentStep beinhalten die aktuelle Inkrement Nr.
ccc   und Step Nr..
c
      integer CNI_ZR(ProbSizeZR,NumOfCPU), 
     *        CNS_ZR(ProbSizeZR,NumOfCPU)
c
      integer CurrentInc, CurrentStep
c
ccc   Die ContMu und CallCount Arrays werden als common Blocks deriniert.
c
      common /KontaktReal/ ContMuZR, CallCountZR, MuSum, MuSumCallCunt
c
ccc   Die Variablen CurrentInc und CurrentStep sowie die Knotenspezifischen
ccc   Arrays CNI und CNS werden als common Blocks definiert.
c
      common /KTime/ CurrentInc, CurrentStep, CNI_ZR, CNS_ZR
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccc           /\ Ende der Variablen Deklarationen /\                ccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
ccc   myThrID wird auf die aktive Thread Nr. gesetzt.
c
      myThrID = get_thread_id() +1
c
ccc   numThreads wird auf die für die Parallelisierung verwendete Anzahl
ccc   von Threads gesetzt.
c
      numThreads = getNumThreads()
c 
ccc   initialisierung der common Block Arrays
c
       if (TIME(i_ufld_Total) .eq. 0.0) then
       do k=1,ProbSizeZR
        do i=1,NumOfCPU
         ContMuZR(k,i)=0.0
         CallCountZR(k,i)=0
         CNI_ZR(k,i)=0
         CNS_ZR(k,i)=0
        end do
       end do
       else
c
ccc   Eine Schleife über alle Knoten, die in diesem Aufrufen bearbeitet
ccc   werden
c
      do k=1,nBlock
c
ccc   tempVar und help muss zu Beginn der Schleife auf 0 zurückgesetzt 
ccc   werden.
c
       tempVar=0.0
       help=0
c
ccc   Eine Schleife über alle CPUs bzw Threadnummern.
c
        do i=1,NumOfCPU
c 
ccc   Damit nur Werte verarbeitet werden, die im letzten Inkrement
ccc   berechnet wurden, wird eine weiter if-Abfrage eingefügt.
c 
         if (CNI_ZR(JNODEID(k),i) .eq. CurrentInc .and.
     *       CNS_ZR(JNODEID(k),i) .eq. CurrentStep) then
c 
ccc 
c 
          help =help+1
c 
ccc   in tempVar werden die Zwischenergebnisse aus der Berechnung
ccc   des Reibkoeffizienten am Masterknoten gespeichert. Dafür
ccc   wird jeweils die Summe der berechnet Reibkoeffizienten durch
ccc   die Anzahl der Berechnungen dividiert.
c 
          tempVar=tempVar+
     *     ContMuZR(JNODEID(k),i)/CallCountZR(JNODEID(k),i)
         end if
        end do
c
ccc   Am Ende der Schleife wird der in tempVar berechnete Wert an die
ccc   Fieldvariable übergeben.
c
       if ( tempVar .ne. 0 ) then
          FIELD(k,1,1)=tempVar/help
       else
          FIELD(k,1,1)=0
       end if
c
ccc   tempVar und help muss zu Beginn der Schleife auf 0 zurückgesetzt 
ccc   werden.
c
       tempVar=0.0
       help=0
c
ccc   Eine Schleife über alle CPUs bzw Threadnummern.
c
        do i=1,NumOfCPU
c 
ccc 
c 
         if (MuSumCallCunt(JNODEID(k),i) .ne. 0) then
          help =help+1
c 
ccc 
c 
          tempVar=tempVar+MuSum(JNODEID(k),i)
     *     /MuSumCallCunt(JNODEID(k),i)
         end if
        end do
       if ( tempVar .ne. 0 ) then
          FIELD(k,1,2)=tempVar/help
       else
          FIELD(k,1,2)=0
       end if
c
      end do
      end if
      RETURN
      END
	  