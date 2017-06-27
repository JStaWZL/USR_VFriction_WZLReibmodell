c
c User subroutine VFRICTION to define friction forces
c
      subroutine vfriction (
c Write only - 
     *   fTangential, 
c Read/Write - 
     *   state,
c Read only - 
     *   nBlock, nBlockAnal, nBlockEdge, 
     *   nNodState, nNodSlv, nNodMst, 
     *   nFricDir, nDir, 
     *   nStates, nProps, nTemp, nFields, 
     *   jFlags, rData, 
     *   surfInt, surfSlv, surfMst, 
     *   jConSlvUid, jConMstUid, props, 
     *   dSlipFric, fStickForce, fTangPrev, fNormal, 
     *   areaCont, dircosN, dircosSl, 
     *   shapeSlv, shapeMst, 
     *   coordSlv, coordMst, 
     *   velSlv, velMst, 
     *   tempSlv, tempMst, 
     *   fieldSlv, fieldMst )
c
c Array dimensioning variables:
c
c      nBlockAnal = nBlock    (non-analytical-rigid master surface)
c      nBlockAnal = 1         (analytical rigid master surface)
c      nBlockEdge = 1         (non-edge-type slave surface)
c      nBlockEdge = nBlock    (edge-type slave surface)
c      nNodState  = 1         (node-type slave surface)
c      nNodState  = 4         (edge-type slave surface)
c      nNodSlv    = 1         (node-type slave surface)
c      nNodSlv    = 2         (edge-type slave surface)
c      nNodMst    = 4         (facet-type master surface)
c      nNodMst    = 2         (edge-type master surface)
c      nNodMst    = 1         (analytical rigid master surface)
c
c Surface names surfSlv and surfMst are not available for
c general case (set to blank).
c
      include 'vaba_param.inc'
c
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
     *   dircosSl(nDir,nBlock), 
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
c
      parameter( iKStep    = 1,
     *           iKInc     = 2,
     *           iLConType = 3,
     *           nFlags    = 3 )
      parameter( iTimStep      = 1,
     *           iTimGlb       = 2,
     *           iDTimCur      = 3,
     *           iFrictionWork = 4, 
     *           nData         = 4 )
c
      dimension jFlags(nFlags), rData(nData)
      character*80 surfInt, surfSlv, surfMst
	  
	 
c
ccc-----------------------------------------------------------------ccc
ccc   Benutzer Code zur Definition der VFRICTION Subroutine         ccc
ccc-----------------------------------------------------------------ccc
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccc                \/  Variablen Deklarationen \/                   ccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
ccc   ProbSize bestimmt die Groeße der common Blocks. 
ccc   ProbSize muss mindestens 2* Anz. der Knoten im zu untersuchenden
ccc   Node Set sein (Bspw. 2* Knoten Anz. des Blechstücks).
c
      integer ProbSize
      parameter (ProbSize=1100000)
	  
c
ccc  Unter firstIntProp und secondIntProp sind die Namen 
ccc  der beiden Interaction Properties des Modells gespeichert  
c
	  character*7 firstIntProp
	  character*9 secondIntProp
	  parameter(firstIntProp = 'INTPROP', secondIntProp = 'INTPROP-2')
c
ccc   KontMu ist das Array zur Uebergabe der Reibungskoeffizienten aus
ccc   der VFRICTION in die VUFIELD
c
      real KontMuO(ProbSize), KontMuU(ProbSize)
c
ccc   WeightedMu dient als Array zur Berechnung eines Durchschnitts-
ccc   wertes der Reibungskoeffizienten
c
      double precision WeightedMuO(ProbSize), WeightedMuU(ProbSize)
c
ccc   Zur Berechnung des Mittelwertes wird die Summe ueber die Zeit-
ccc   inkremente genutzt, bei der ein Reibungskoeffizient ungleich
ccc   0 berechnet wurde
c
      real TimeSumO(ProbSize), TimeSumU(ProbSize)
c
ccc   Einige lokale Variablen zur Berechnung und Definition der 
ccc   physikalischen Parameter: p, v, t, m
c
      real c, p_exp, v_exp, t_exp, p, v, t, m, ft, disp
      real p_crit, v_crit, t_crit
      parameter (p_crit=1.0, v_crit=10.0, t_crit=20.0) 	  
c      
ccc   Struktur der "props" (definition in .inp bzw. CAE)
ccc   props(1) - case
ccc   props(2) - c
ccc   props(3) - p_exp
ccc   props(4) - v_exp
ccc   props(5) - t_exp
ccc   props(6) - m_exp
      parameter( iProps_case   = 1,
     *           iProps_c  = 2,
     *           iProps_p_exp  = 3,
     *           iProps_t_exp  = 4, 
     *           iProps_v_exp  = 5,
     *           iProps_m_exp  = 6 )
ccc
ccc   !!!!!!!!!!!!!!!!!!!!!!!    WICHTIG    !!!!!!!!!!!!!!!!!!!!!!!!!!!!
ccc
ccc   Es muessen alle 6 Variablen fuer jedes case definiert werden.
ccc   Falls in einem der cases nicht alle variablen noetig sind, muss
ccc   trotzdem ein dummy-Wert definiert werden. Er hat keinen Einfluss
ccc   auf die Berechnungen.
c     -----------------------------------------------------------------
ccc   KurentNodeInc und KurentNodeStep zeigen von welchem Inkrement und
ccc   Schritt die Reibungskoeffizienten in KuntMu stammen.
ccc   KurentInc und KurentStep beinhalten die aktuelle Inkrement Nr.
ccc   und Step Nr..
c
      integer KurentNodeIncO(ProbSize), KurentNodeStepO(ProbSize), 
     *        KurentInc, KurentStep,
     *        KurentNodeIncU(ProbSize), KurentNodeStepU(ProbSize)
c
ccc   KontMu, KurentNodeInc, KurentNodeStep, KurentInc, KurentStep und
ccc   HistoryOpt werden als common Blocks definiert.
c
      common /KontaktReal/ KontMuO, WeightedMuO, TimeSumO, KontMuU,
     *                     WeightedMuU, TimeSumU
      common /KTime/ KurentNodeIncO, KurentInc, KurentNodeStepO, 
     *               KurentStep, KurentNodeIncU, KurentNodeStepU
c
ccc save Befehl stellt sicher, dass die common Blocks nach verlassen
ccc der Subroutine erhalten bleibt.
c
      save /KontaktReal/
      save /KTime/
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccc           /\ Ende der Variablen Deklarationen /\                ccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
ccc   Die aktuelle Inkrement Nr. und Schritt Nr. wird upgedatet
c
      KurentInc=jFlags(2)
      KurentStep=jFlags(1)
c
ccc   Die gewaehlte Berechnungsvorschrift fuer die 
ccc   Reibungskoeffizienten werden aus der Abaqus Inputfile an
ccc   case in Form eines numerischen Wertes uebergeben.
      case =props(iProps_case)
ccc   Definition der Hifsvariablen fuer die Vereinfachung der Formeln      
      c  =  props(iProps_c)
      p_exp=props(iProps_p_exp)
      v_exp=props(iProps_t_exp)
      t_exp=props(iProps_v_exp)
      m_exp=props(iProps_m_exp)    

c
       if ( case .eq. 1) then
ccc-----------------------------------------------------------------ccc
ccc   Reibungskoeffizient wird druckabhaengig berechnet             ccc
ccc-----------------------------------------------------------------ccc
         do k = 1, nBlock
            if (fNormal(k) .ne. 0.0) then
               p=fNormal(k)/areaCont(k)
               if (p.LT.p_crit) p=p_crit			   
               if (surfInt .eq. firstIntProp) then
                  KontMuO(jConSlvUid(1,k))=c*p**p_exp
                  WeightedMuO(jConSlvUid(1,k))=
     *               WeightedMuO(jConSlvUid(1,k))+
     *               KontMuO(jConSlvUid(1,k))*
     *               rData(3)
                  TimeSumO(jConSlvUid(1,k))=TimeSumO(jConSlvUid(1,k))+
     *               rData(3)
                  ft = min ( KontMuO(jConSlvUid(1,k))*fNormal(k), 
     *               fStickForce(k) )
               else
                  KontMuU(jConSlvUid(1,k))=c*p**p_exp
                  WeightedMuU(jConSlvUid(1,k))=
     *               WeightedMuU(jConSlvUid(1,k))+
     *               KontMuU(jConSlvUid(1,k))*
     *               rData(3)
                  TimeSumU(jConSlvUid(1,k))=TimeSumU(jConSlvUid(1,k))+
     *               rData(3)
                  ft = min ( KontMuU(jConSlvUid(1,k))*fNormal(k), 
     *               fStickForce(k) )
               end if
               fTangential(1,k) = -ft
               fTangential(2,k) = 0.0
            else
               if (surfInt .eq. firstIntProp) then
                  KontMuO(jConSlvUid(1,k)) = 0.0
               else
                  KontMuU(jConSlvUid(1,k)) = 0.0
               end if
            end if
            if (surfInt .eq. firstIntProp) then
               KurentNodeIncO(jConSlvUid(1,k))=jFlags(2)
               KurentNodeStepO(jConSlvUid(1,k))=jFlags(1)
            else
               KurentNodeIncU(jConSlvUid(1,k))=jFlags(2)
               KurentNodeStepU(jConSlvUid(1,k))=jFlags(1)
            end if
         end do
      else if( case .eq. 2) then
ccc-----------------------------------------------------------------ccc
ccc   Reibungskoeffizient wird rel. geschwindigkeitsabhaengig       ccc
ccc   berechnet                                                     ccc
ccc-----------------------------------------------------------------ccc
         do k = 1, nBlock
            disp=sqrt(dSlipFric(1,k)**2.0 +
     *                dSlipFric(2,k)**2.0 +
     *                dSlipFric(3,k)**2.0)
            if (fNormal(k)      .ne. 0.0 .and.
     *          rData(iDTimCur) .ne. 0.0 .and.
     *          disp            .ne. 0.0) then
               v=disp/rData(iDTimCur)
               if (v.LT.v_crit) v=v_crit
               if (surfInt .eq. firstIntProp) then
                  KontMuO(jConSlvUid(1,k))=c*v**v_exp
                  WeightedMuO(jConSlvUid(1,k))=
     *               WeightedMuO(jConSlvUid(1,k))+
     *               KontMuO(jConSlvUid(1,k))*
     *               rData(3)
                  TimeSumO(jConSlvUid(1,k))=TimeSumO(jConSlvUid(1,k))+
     *               rData(3)
                  ft = min ( KontMuO(jConSlvUid(1,k))*fNormal(k), 
     *               fStickForce(k) )
               else
                  KontMuO(jConSlvUid(1,k))=c*v**v_exp
                  WeightedMuO(jConSlvUid(1,k))=
     *               WeightedMuO(jConSlvUid(1,k))+
     *               KontMuO(jConSlvUid(1,k))*
     *               rData(3)
                  TimeSumO(jConSlvUid(1,k))=TimeSumO(jConSlvUid(1,k))+
     *               rData(3)
                  ft = min ( KontMuO(jConSlvUid(1,k))*fNormal(k), 
     *               fStickForce(k) )
               end if
               fTangential(1,k) = -ft
               fTangential(2,k) = 0.0
            else
               if (surfInt .eq. firstIntProp) then
                  KontMuO(jConSlvUid(1,k)) = 0.0
               else
                  KontMuU(jConSlvUid(1,k)) = 0.0
               end if
            end if
            if (surfInt .eq. firstIntProp) then
               KurentNodeIncO(jConSlvUid(1,k))=jFlags(2)
               KurentNodeStepO(jConSlvUid(1,k))=jFlags(1)
            else
               KurentNodeIncU(jConSlvUid(1,k))=jFlags(2)
               KurentNodeStepU(jConSlvUid(1,k))=jFlags(1)
            end if
         end do
      else if( case .eq. 3) then
ccc-----------------------------------------------------------------ccc
ccc   Reibungskoeffizient wird temperaturabhaengig berechnet.       ccc
ccc-----------------------------------------------------------------ccc
         do k = 1, nBlock
            if (fNormal(k) .ne. 0.0) then
              t=(tempMst(k)+tempSlv(k))/2.0
              if (t.LT.t_crit) t=t_crit
                if (surfInt .eq. firstIntProp) then
                  KontMuO(jConSlvUid(1,k))=c*t**t_exp
                  WeightedMuO(jConSlvUid(1,k))=
     *               WeightedMuO(jConSlvUid(1,k))+
     *               KontMuO(jConSlvUid(1,k))*
     *               rData(3)
                  TimeSumO(jConSlvUid(1,k))=TimeSumO(jConSlvUid(1,k))+
     *               rData(3)
                  ft = min ( KontMuO(jConSlvUid(1,k))*fNormal(k), 
     *                    fStickForce(k) )
               else
                  KontMuU(jConSlvUid(1,k))=c*t**t_exp
                  WeightedMuU(jConSlvUid(1,k))=
     *               WeightedMuU(jConSlvUid(1,k))+
     *               KontMuU(jConSlvUid(1,k))*
     *               rData(3)
                  TimeSumU(jConSlvUid(1,k))=TimeSumU(jConSlvUid(1,k))+
     *               rData(3)
                  ft = min ( KontMuU(jConSlvUid(1,k))*fNormal(k), 
     *               fStickForce(k) )
               end if
               fTangential(1,k) = -ft
               fTangential(2,k) = 0.0
            else
               if (surfInt .eq. firstIntProp) then
                  KontMuO(jConSlvUid(1,k)) = 0.0
               else
                  KontMuU(jConSlvUid(1,k)) = 0.0
               end if
            end if
            if (surfInt .eq. firstIntProp) then
               KurentNodeIncO(jConSlvUid(1,k))=jFlags(2)
               KurentNodeStepO(jConSlvUid(1,k))=jFlags(1)
            else
               KurentNodeIncU(jConSlvUid(1,k))=jFlags(2)
               KurentNodeStepU(jConSlvUid(1,k))=jFlags(1)
            end if
         end do
      else if( case .eq. 4) then
ccc-----------------------------------------------------------------ccc
ccc   Reibungskoeffizient wird druck- und rel                       ccc
ccc   geschwindigkeitsabhaengig berechnet.                          ccc
ccc-----------------------------------------------------------------ccc
         do k = 1, nBlock
            disp=sqrt(dSlipFric(1,k)**2.0 +
     *                dSlipFric(2,k)**2.0 +
     *                dSlipFric(3,k)**2.0)
            if (fNormal(k)      .ne. 0.0 .and.
     *          areaCont(k)     .ne. 0.0 .and.
     *          rData(iDTimCur) .ne. 0.0 .and.
     *          disp            .ne. 0.0) then
               p=fNormal(k)/areaCont(k)
               v=disp/rData(iDTimCur)	 
               if (surfInt .eq. firstIntProp) then
                  KontMuO(jConSlvUid(1,k))=c*p**p_exp
     *                                      *v**v_exp
                  WeightedMuO(jConSlvUid(1,k))=
     *               WeightedMuO(jConSlvUid(1,k))+
     *               KontMuO(jConSlvUid(1,k))*
     *               rData(3)
                  TimeSumO(jConSlvUid(1,k))=TimeSumO(jConSlvUid(1,k))+
     *                                     rData(3)
                  ft = min ( KontMuO(jConSlvUid(1,k))*fNormal(k), 
     *                       fStickForce(k) )
               else
                  KontMuU(jConSlvUid(1,k))=c*p**p_exp
     *                                      *v**v_exp
                  WeightedMuU(jConSlvUid(1,k))=
     *               WeightedMuU(jConSlvUid(1,k))+
     *               KontMuU(jConSlvUid(1,k))*
     *               rData(3)
                  TimeSumU(jConSlvUid(1,k))=TimeSumU(jConSlvUid(1,k))+
     *                                     rData(3)
                  ft = min ( KontMuU(jConSlvUid(1,k))*fNormal(k), 
     *                       fStickForce(k) )
                  end if
               fTangential(1,k) = -ft
               fTangential(2,k) = 0.0
            else
               if (surfInt .eq. firstIntProp) then
                  KontMuO(jConSlvUid(1,k)) = 0.0
               else
                  KontMuU(jConSlvUid(1,k)) = 0.0
               end if
            end if
            if (surfInt .eq. firstIntProp) then
               KurentNodeIncO(jConSlvUid(1,k))=jFlags(2)
               KurentNodeStepO(jConSlvUid(1,k))=jFlags(1)
            else
               KurentNodeIncU(jConSlvUid(1,k))=jFlags(2)
               KurentNodeStepU(jConSlvUid(1,k))=jFlags(1)
            end if
         end do
      else if(case .eq. 5) then
ccc-----------------------------------------------------------------ccc
ccc   Reibungskoeffizient wird druck - und                          ccc
ccc   temperaturabhaengig berechnet.                                ccc
ccc-----------------------------------------------------------------ccc
         do k = 1, nBlock
            if (fNormal(k) .ne. 0.0) then
              p=fNormal(k)/areaCont(k)
              t=(tempMst(k)+tempSlv(k))/2.0
              if (p.LT.p_crit) p=p_crit
              if (t.LT.t_crit) t=t_crit			  
              if (surfInt .eq. firstIntProp) then    
                  KontMuO(jConSlvUid(1,k))=c*p**p_exp
     *                                      *t**t_exp
                  WeightedMuO(jConSlvUid(1,k))=
     *               WeightedMuO(jConSlvUid(1,k))+
     *               KontMuO(jConSlvUid(1,k))*
     *               rData(3)
                  TimeSumO(jConSlvUid(1,k))=TimeSumO(jConSlvUid(1,k))+
     *               rData(3)
                  ft = min ( KontMuO(jConSlvUid(1,k))*fNormal(k), 
     *               fStickForce(k) )
               else
                  p=fNormal(k)/areaCont(k)
                  KontMuU(jConSlvUid(1,k))=c*p**p_exp
     *                                      *t**t_exp
                  WeightedMuU(jConSlvUid(1,k))=
     *               WeightedMuU(jConSlvUid(1,k))+
     *               KontMuU(jConSlvUid(1,k))*
     *               rData(3)
                  TimeSumU(jConSlvUid(1,k))=TimeSumU(jConSlvUid(1,k))+
     *               rData(3)
                  ft = min ( KontMuU(jConSlvUid(1,k))*fNormal(k), 
     *               fStickForce(k) )
               end if
               fTangential(1,k) = -ft
               fTangential(2,k) = 0.0
            else
               if (surfInt .eq. firstIntProp) then
                  KontMuO(jConSlvUid(1,k)) = 0.0
               else
                  KontMuU(jConSlvUid(1,k)) = 0.0
               end if
            end if
            if (surfInt .eq. firstIntProp) then
               KurentNodeIncO(jConSlvUid(1,k))=jFlags(2)
               KurentNodeStepO(jConSlvUid(1,k))=jFlags(1)
            else
               KurentNodeIncU(jConSlvUid(1,k))=jFlags(2)
               KurentNodeStepU(jConSlvUid(1,k))=jFlags(1)
            end if
         end do
      else if(case .eq. 6) then
ccc-----------------------------------------------------------------ccc
ccc   Reibungskoeffizient wird geschwindigkeits- und                ccc
ccc   temperaturabhaengig berechnet.                                ccc
ccc-----------------------------------------------------------------ccc
         do k = 1, nBlock
            disp=sqrt(dSlipFric(1,k)**2.0 +
     *                dSlipFric(2,k)**2.0 +
     *                dSlipFric(3,k)**2.0)
            if (fNormal(k)      .ne. 0.0 .and.
     *          rData(iDTimCur) .ne. 0.0 .and.
     *          disp            .ne. 0.0) then
               v=disp/rData(iDTimCur)
               t=(tempMst(k)+tempSlv(k))/2.0
               if (v.LT.v_crit) v=v_crit
               if (t.LT.t_crit) t=t_crit	 
               if (surfInt .eq. firstIntProp) then
                  KontMuO(jConSlvUid(1,k))=c*v**v_exp
     *                                      *t**t_exp
                  WeightedMuO(jConSlvUid(1,k))=
     *               WeightedMuO(jConSlvUid(1,k))+
     *               KontMuO(jConSlvUid(1,k))*
     *               rData(3)
                  TimeSumO(jConSlvUid(1,k))=TimeSumO(jConSlvUid(1,k))+
     *               rData(3)
                  ft = min ( KontMuO(jConSlvUid(1,k))*fNormal(k), 
     *                       fStickForce(k) )
               else
                  KontMuU(jConSlvUid(1,k))=c*v**v_exp
     *                                      *t**t_exp
                  WeightedMuU(jConSlvUid(1,k))=
     *               WeightedMuU(jConSlvUid(1,k))+
     *               KontMuU(jConSlvUid(1,k))*
     *               rData(3)
                  TimeSumU(jConSlvUid(1,k))=TimeSumU(jConSlvUid(1,k))+
     *               rData(3)
                  ft = min ( KontMuU(jConSlvUid(1,k))*fNormal(k), 
     *                       fStickForce(k) )
               end if
               fTangential(1,k) = -ft
               fTangential(2,k) = 0.0
            else
               if (surfInt .eq. firstIntProp) then
                  KontMuO(jConSlvUid(1,k)) = 0.0
               else
                  KontMuU(jConSlvUid(1,k)) = 0.0
               end if
            end if
            if (surfInt .eq. firstIntProp) then
               KurentNodeIncO(jConSlvUid(1,k))=jFlags(2)
               KurentNodeStepO(jConSlvUid(1,k))=jFlags(1)
            else
               KurentNodeIncU(jConSlvUid(1,k))=jFlags(2)
               KurentNodeStepU(jConSlvUid(1,k))=jFlags(1)
            end if
         end do
      else if(case .eq. 7) then
ccc-----------------------------------------------------------------ccc
ccc   Reibungskoeffizient wird druck-, rel. geschwindigkeits- und   ccc
ccc   temperaturabhaengig berechnet.                                ccc
ccc-----------------------------------------------------------------ccc
         do k = 1, nBlock
            disp=sqrt(dSlipFric(1,k)**2.0 +
     *                dSlipFric(2,k)**2.0 +
     *                dSlipFric(3,k)**2.0)
            if (fNormal(k)      .ne. 0.0 .and.
     *          rData(iDTimCur) .ne. 0.0 .and.
     *          disp            .ne. 0.0 ) then
               p=fNormal(k)/areaCont(k)
               v=disp/rData(iDTimCur)
               t=(tempMst(k)+tempSlv(k))/2.0
               if (p.LT.p_crit) p=p_crit
               if (v.LT.v_crit) v=v_crit
               if (t.LT.t_crit) t=t_crit	 
               if (surfInt .eq. firstIntProp) then
                  KontMuO(jConSlvUid(1,k))=c*p**p_exp
     *                                      *v**v_exp
     *                                      *t**t_exp
                  WeightedMuO(jConSlvUid(1,k))=
     *               WeightedMuO(jConSlvUid(1,k))+
     *               KontMuO(jConSlvUid(1,k))*
     *               rData(3)
                  TimeSumO(jConSlvUid(1,k))=TimeSumO(jConSlvUid(1,k))+
     *               rData(3)
                  ft = min ( KontMuO(jConSlvUid(1,k))*fNormal(k), 
     *               fStickForce(k) )
               else if (surfInt .eq. secondIntProp) then
                  KontMuU(jConSlvUid(1,k))=c*p**p_exp
     *                                      *v**v_exp
     *                                      *t**t_exp
                  WeightedMuU(jConSlvUid(1,k))=
     *               WeightedMuU(jConSlvUid(1,k))+
     *               KontMuU(jConSlvUid(1,k))*
     *               rData(3)
                  TimeSumU(jConSlvUid(1,k))=TimeSumU(jConSlvUid(1,k))+
     *               rData(3)
                  ft = min ( KontMuU(jConSlvUid(1,k))*fNormal(k), 
     *               fStickForce(k) )
               end if
               fTangential(1,k) = -ft
               fTangential(2,k) = 0.0
            else 
               if (surfInt .eq. firstIntProp) then
                  KontMuO(jConSlvUid(1,k)) = 0.0
               else if (surfInt .eq. secondIntProp) then
                  KontMuU(jConSlvUid(1,k)) = 0.0
               end if
            end if
            if (surfInt .eq. firstIntProp) then
               KurentNodeIncO(jConSlvUid(1,k))=jFlags(2)
               KurentNodeStepO(jConSlvUid(1,k))=jFlags(1)
            else if (surfInt .eq. secondIntProp) then
               KurentNodeIncU(jConSlvUid(1,k))=jFlags(2)
               KurentNodeStepU(jConSlvUid(1,k))=jFlags(1)
            end if
         end do
      else if(case .eq. 8) then
ccc-----------------------------------------------------------------ccc
ccc   Reibungskoeffizient wird druck-, rel. geschwindigkeits-,      ccc
ccc   temperatur-, schmierstoffmengenabhaengig berechnet            ccc
ccc-----------------------------------------------------------------ccc
         do k = 1, nBlock
            disp=sqrt(dSlipFric(1,k)**2.0 +
     *                dSlipFric(2,k)**2.0 +
     *                dSlipFric(3,k)**2.0)
            if (fNormal(k)      .ne. 0.0 .and.
     *          rData(iDTimCur) .ne. 0.0 .and.
     *          disp            .ne. 0.0 ) then
               p=fNormal(k)/areaCont(k)
               v=disp/rData(iDTimCur)
               t=(tempMst(k)+tempSlv(k))/2.0

               m=2.0			   
               if (p.LT.p_crit) p=p_crit
               if (v.LT.v_crit) v=v_crit
               if (t.LT.t_crit) t=t_crit	 
               if (surfInt .eq. firstIntProp) then
                  KontMuO(jConSlvUid(1,k))=c*p**p_exp
     *                                      *v**v_exp
     *                                      *t**t_exp
     *                                      *m**m_exp
                  WeightedMuO(jConSlvUid(1,k))=
     *               WeightedMuO(jConSlvUid(1,k))+
     *               KontMuO(jConSlvUid(1,k))*
     *               rData(3)
                  TimeSumO(jConSlvUid(1,k))=TimeSumO(jConSlvUid(1,k))+
     *               rData(3)
                  ft = min ( KontMuO(jConSlvUid(1,k))*fNormal(k), 
     *               fStickForce(k) )
               else
                  KontMuU(jConSlvUid(1,k))=c*p**p_exp
     *                                      *v**v_exp
     *                                      *t**t_exp
     *                                      *m**m_exp
                  WeightedMuU(jConSlvUid(1,k))=
     *               WeightedMuU(jConSlvUid(1,k))+
     *               KontMuU(jConSlvUid(1,k))*
     *               rData(3)
                  TimeSumU(jConSlvUid(1,k))=TimeSumU(jConSlvUid(1,k))+
     *               rData(3)
                  ft = min ( KontMuU(jConSlvUid(1,k))*fNormal(k), 
     *               fStickForce(k) )
               end if
               fTangential(1,k) = -ft
               fTangential(2,k) = 0.0
            else
               if (surfInt .eq. firstIntProp) then
                  KontMuO(jConSlvUid(1,k)) = 0.0
               else
                  KontMuU(jConSlvUid(1,k)) = 0.0
               end if
            end if
            if (surfInt .eq. firstIntProp) then
               KurentNodeIncO(jConSlvUid(1,k))=jFlags(2)
               KurentNodeStepO(jConSlvUid(1,k))=jFlags(1)
            else if (surfInt .eq. secondIntProp) then
               KurentNodeIncU(jConSlvUid(1,k))=jFlags(2)
               KurentNodeStepU(jConSlvUid(1,k))=jFlags(1)
            end if
         end do		 
      end if
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
c
ccc-----------------------------------------------------------------ccc
ccc   Benutzer Code zur Definition der VUFIELD Subroutine           ccc
ccc-----------------------------------------------------------------ccc
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccc                \/  Variablen Deklarationen \/                   ccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
ccc   ProbSize bestimmt die Groeße der common Blocks. 
ccc   ProbSize muss mindestens 2* Anz. der Knoten im zu untersuchenden
ccc   Node Set sein (Bspw. 2* Knoten Anz. des Blechwerkstuecks).
c
      integer ProbSize
      parameter (ProbSize=1100000)
c
ccc   KontMu ist das Array zur Uebergabe der Reibungskoeffizienten aus
ccc   der VFRICTION in die VUFIELD
c
      real KontMuO(ProbSize), KontMuU(ProbSize)
c
ccc   WeightedMu dient als Array zur Berechnung eines Durchschnitts-
ccc   wertes der Reibungskoeffizienten
c
      double precision WeightedMuO(ProbSize), WeightedMuU(ProbSize)
c
ccc   Zur Berechnung des Mittelwertes wird die Summe ueber die Zeit-
ccc   inkremente genutzt, bei der ein Reibungskoeffizient ungleich
ccc   0 berechnet wurde
c
      real TimeSumO(ProbSize), TimeSumU(ProbSize)
c
ccc   KurentNodeInc und KurentNodeStep zeigen von welchem Inkrement und
ccc   Schritt die Reibungskoeffizienten in KuntMu stammen.
ccc   KurentInc und KurentStep beinhalten die aktuelle Inkrement Nr.
ccc   und Step Nr..
c
      integer KurentNodeIncO(ProbSize), KurentNodeStepO(ProbSize), 
     *        KurentInc, KurentStep,
     *        KurentNodeIncU(ProbSize), KurentNodeStepU(ProbSize)
c   
ccc   KontMu, KurentNodeInc, KurentNodeStep, KurentInc, KurentStep und
ccc   WeightedMu werden als common Blocks definiert.
c
      common /KontaktReal/ KontMuO, WeightedMuO, TimeSumO, KontMuU,
     *                     WeightedMuU, TimeSumU
      common /KTime/ KurentNodeIncO, KurentInc, KurentNodeStepO, 
     *               KurentStep, KurentNodeIncU, KurentNodeStepU
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccc           /\ Ende der Variablen Deklarationen /\                ccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
ccc Initialisierung der Array KontMu, WeightedMu und KurentNodeInc
ccc zur Zeit des ersten Inkrements der Rechnung
c
      if (TIME(i_ufld_Total) .eq. 0.0) then
         do k = 1, ProbSize
            KontMuO(k) = 0.0
            KurentNodeIncO(k) = 0
            KurentNodeStepO(k) = 1
            WeightedMuO(k) = 0.0
            TimeSumO(k) = 0.0
            KontMuU(k) = 0.0
            KurentNodeIncU(k) = 0
            KurentNodeStepU(k) = 1
            WeightedMuU(k) = 0.0
            TimeSumU(k) = 0.0			
         end do
      end if
c
ccc   Update der Field Variable zur Erzeugung des Konturplots
c
      if (TIME(i_ufld_Total) .ne. 0.0) then
c
ccc      In der ersten Field Variable werden die Reibungskoeffizienten
ccc      mit Historie uebergeben
c
        if (JFLAGS(i_ufld_kPass) .eq. 1) then
         do k = 1,NBLOCK
            FIELD(k, 1, 1) = KontMuO(JNODEID(k))
         end do
c
ccc      In der zweiten Field Variable werden die Reibungskoeffizienten
ccc      ohne Historie uebertragen
c
         do k = 1,NBLOCK
            if (KurentNodeIncO(JNODEID(k)) .eq. KurentInc .and.
     *         KurentNodeStepO(JNODEID(k)) .eq. KurentStep ) then
               FIELD(k, 1, 2) = KontMuO(JNODEID(k)) 
            else
               FIELD(k, 1, 2) = 0.0
            end if
         end do
c
ccc      In der dritten Field Variable werden die maximalen
ccc      Reibungskoeffizienten gespeichert
c
         do k= 1,NBLOCK
            if (FIELD(k, 1, 3) .lt. KontMuO(JNODEID(k))) then
               FIELD(k, 1, 3)=KontMuO(JNODEID(k))
            end if
         end do
c
ccc      In der vierten Field Variable werden die Reibungs-
ccc      koeffizienten nur über den Zeitbereich gemittelt, in dem sie
ccc      ungleich 0 sind, also Kontakt besteht
c
         do k= 1,NBLOCK
            if (TimeSumO(JNODEID(k)) .ne. 0.0) then 
               FIELD(k, 1, 4)=WeightedMuO(JNODEID(k))/
     *            TimeSumO(JNODEID(k))
            else
               FIELD(k, 1, 4)=0.0
            end if
         end do
c
ccc      In der ersten Field Variable werden die Reibungskoeffizienten
ccc      mit Historie uebergeben
c
         do k = 1,NBLOCK
            FIELD(k, 1, 5) = KontMuU(JNODEID(k))
         end do
c
ccc      In der zweiten Field Variable werden die Reibungskoeffizienten
ccc      ohne Historie uebertragen
c
         do k = 1,NBLOCK
            if (KurentNodeIncU(JNODEID(k)) .eq. KurentInc .and.
     *         KurentNodeStepU(JNODEID(k)) .eq. KurentStep ) then
               FIELD(k, 1, 6) = KontMuU(JNODEID(k)) 
            else
               FIELD(k, 1, 6) = 0.0
            end if
         end do
c
ccc      In der dritten Field Variable werden die maximalen
ccc      Reibungskoeffizienten gespeichert
c
         do k= 1,NBLOCK
            if (FIELD(k, 1, 7) .lt. KontMuU(JNODEID(k))) then
               FIELD(k, 1, 7)=KontMuU(JNODEID(k))
            end if
         end do
c
ccc      In der vierten Field Variable werden die Reibungs-
ccc      koeffizienten nur über den Zeitbereich gemittelt, in dem sie
ccc      ungleich 0 sind, also Kontakt besteht
c
         do k= 1,NBLOCK
            if (TimeSumU(JNODEID(k)) .ne. 0.0) then 
               FIELD(k, 1, 8)=WeightedMuU(JNODEID(k))/
     *            TimeSumU(JNODEID(k))
            else
               FIELD(k, 1, 8)=0.0
            end if
         end do
        else
         do j=1,NFIELD
          do k=1,NBLOCK
             FIELD(k,1,j)=0.0
          end do
         end do
        end if
      end if
      RETURN
      END