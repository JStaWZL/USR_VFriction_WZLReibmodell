# -*- coding: iso-8859-1 -*-
from abaqus import *
from abaqusConstants import *
from Tkinter import *
import ttk
import tkMessageBox
msg=tkMessageBox
import multiprocessing
import section
import regionToolset
import displayGroupMdbToolset as dgm
import part
import material
import assembly
import step
import interaction
import load
import mesh
import optimization
import job
import sketch
import visualization
import xyPlot
import displayGroupOdbToolset as dgo
import connectorBehavior

#unicode c,alpha,beta,gamma,delta
fields = 'c',u'\u03B1',u'\u03B2',u'\u03B3',u'\u03B4'
filedir=['Model','Interaction Properties','2nd Interaction Properties']
filedir2=['Node Set']
filedir3=['Job Name']
DefNames=['Model-1','Friction','Friction2']
DefNames2=['Stück']
DefNames3=['Job1']
entries_out =[]
USRcase = IntVar
BlnDelMods = BooleanVar
BlnDelMods = False
currentModel=""

############################TOOLTIP
import Tkinter

class ToolTip:
    def __init__(self, master, text='Your text here', delay=1500, **opts):
        self.master = master
        self._opts = {'anchor':'center', 'bd':1, 'bg':'lightyellow', 'delay':delay, 'fg':'black',\
                      'follow_mouse':0, 'font':None, 'justify':'left', 'padx':4, 'pady':2,\
                      'relief':'solid', 'state':'normal', 'text':text, 'textvariable':None,\
                      'width':0, 'wraplength':300}
        self.configure(**opts)
        self._tipwindow = None
        self._id = None
        self._id1 = self.master.bind("<Enter>", self.enter, '+')
        self._id2 = self.master.bind("<Leave>", self.leave, '+')
        self._id3 = self.master.bind("<ButtonPress>", self.leave, '+')
        self._follow_mouse = 0
        if self._opts['follow_mouse']:
            self._id4 = self.master.bind("<Motion>", self.motion, '+')
            self._follow_mouse = 1
    
    def configure(self, **opts):
        for key in opts:
            if self._opts.has_key(key):
                self._opts[key] = opts[key]
            else:
                KeyError = 'KeyError: Unknown option: "%s"' %key
                raise KeyError
    
    ##----these methods handle the callbacks on "<Enter>", "<Leave>" and "<Motion>"---------------##
    ##----events on the parent widget; override them if you want to change the widget's behavior--##
    
    def enter(self, event=None):
        self._schedule()
        
    def leave(self, event=None):
        self._unschedule()
        self._hide()
    
    def motion(self, event=None):
        if self._tipwindow and self._follow_mouse:
            x, y = self.coords()
            self._tipwindow.wm_geometry("+%d+%d" % (x, y))
    
    ##------the methods that do the work:---------------------------------------------------------##
    
    def _schedule(self):
        self._unschedule()
        if self._opts['state'] == 'disabled':
            return
        self._id = self.master.after(self._opts['delay'], self._show)

    def _unschedule(self):
        id = self._id
        self._id = None
        if id:
            self.master.after_cancel(id)

    def _show(self):
        if self._opts['state'] == 'disabled':
            self._unschedule()
            return
        if not self._tipwindow:
            self._tipwindow = tw = Tkinter.Toplevel(self.master)
            # hide the window until we know the geometry
            tw.withdraw()
            tw.wm_overrideredirect(1)

            if tw.tk.call("tk", "windowingsystem") == 'aqua':
                tw.tk.call("::tk::unsupported::MacWindowStyle", "style", tw._w, "help", "none")

            self.create_contents()
            tw.update_idletasks()
            x, y = self.coords()
            tw.wm_geometry("+%d+%d" % (x, y))
            tw.deiconify()
    
    def _hide(self):
        tw = self._tipwindow
        self._tipwindow = None
        if tw:
            tw.destroy()
                
    ##----these methods might be overridden in derived classes:----------------------------------##
    
    def coords(self):
        # The tip window must be completely outside the master widget;
        # otherwise when the mouse enters the tip window we get
        # a leave event and it disappears, and then we get an enter
        # event and it reappears, and so on forever :-(
        # or we take care that the mouse pointer is always outside the tipwindow :-)
        tw = self._tipwindow
        twx, twy = tw.winfo_reqwidth(), tw.winfo_reqheight()
        w, h = tw.winfo_screenwidth(), tw.winfo_screenheight()
        # calculate the y coordinate:
        if self._follow_mouse:
            y = tw.winfo_pointery() + 20
            # make sure the tipwindow is never outside the screen:
            if y + twy > h:
                y = y - twy - 30
        else:
            y = self.master.winfo_rooty() + self.master.winfo_height() + 3
            if y + twy > h:
                y = self.master.winfo_rooty() - twy - 3
        # we can use the same x coord in both cases:
        x = tw.winfo_pointerx() - twx / 2
        if x < 0:
            x = 0
        elif x + twx > w:
            x = w - twx
        return x, y

    def create_contents(self):
        opts = self._opts.copy()
        for opt in ('delay', 'follow_mouse', 'state'):
            del opts[opt]
        label = Tkinter.Label(self._tipwindow, **opts)
        label.pack()
############################

def del_Old_Edits():
    print "Deleting older edits..."
    try:
        mdb.models[currentModel].keywordBlock.setValues(edited = 0)
    except Exception,e:
        print "Error",
        print e
        msg.showerror("Error","An unknown error has occurred while discarding all edits in the inp.file.")
        return False
    else: 
        print "Done."
        return True

def GetKeywordPosition(modelName, blockPrefix, occurrence=1):
    #Keyblock-Nummer herausfinden
    if blockPrefix == '':
        return len(mdb.models[modelName].keywordBlock.sieBlocks)+1
    pos = 0
    foundCount = 0
    for block in mdb.models[modelName].keywordBlock.sieBlocks:
        if blockPrefix in block[0:len(blockPrefix)]:
            foundCount = foundCount + 1
            if foundCount >= occurrence:
                return pos
        pos=pos+1
    return -1
 
def USR_Properties(entries):
    #Werte in Input Property eintragen
    Model = entries[0][1].encode('iso-8859-1', 'replace')
    Prop1 = entries[1][1].encode('iso-8859-1', 'replace')
    Prop2 = entries[2][1].encode('iso-8859-1', 'replace')
    case = entries[5][1]
    c = entries[6][1]
    p = entries[7][1]
    v = entries[8][1]
    t = entries[9][1]
    m = entries[10][1]
    CoS = entries[11][1]
    isTool = entries[13][1]
    print "Sending properties to Abaqus CAE..."
    try:
        if(isTool==1):
            CoS=1
            mdb.models[Model].interactionProperties[Prop1].tangentialBehavior.setValues(
            formulation=USER_DEFINED, nStateDependentVars=0, useProperties=ON, 
            table=((c, ), (p, ), (v, ), (t, ), (m, )))
        else:
            mdb.models[Model].interactionProperties[Prop1].tangentialBehavior.setValues(
            formulation=USER_DEFINED, nStateDependentVars=0, useProperties=ON, 
            table=((case, ), (c, ), (p, ), (v, ), (t, ), (m, )))
        if(CoS==2):
            mdb.models[Model].interactionProperties[Prop2].tangentialBehavior.setValues(
            formulation=USER_DEFINED, nStateDependentVars=0, useProperties=ON, 
            table=((case, ), (c, ), (p, ), (v, ), (t, ), (m, )))
    except Exception,e:
        print "Error:",
        if(str(e)=="'%s'"%Model):
            print "Model not found."
            msg.showerror("Model not found","The Model you picked doesn't exist or can't be opened. Please check your Model Assignments.")
        elif(str(e)=="'%s'"%Prop1 or str(e)=="'%s'"%Prop2):
            print "Input Property not found."
            msg.showerror("Input Property not found","The Input Property you picked doesn't exist or can't be opened. Please check your Model Assignments.")
        else:    
            msg.showerror("Error","""An unknown error has occurred. Please check your Model Assignments.
A possible error could be that tangential behavior doesn't exist in the Interaction Property.""")
            print e
        return False
    else:
        print "Done."
    return True

def ModInpFile(entries):
    Model = entries[0][1].encode('iso-8859-1', 'replace')
    Prop1 = entries[1][1].encode('iso-8859-1', 'replace')
    Prop2 = entries[2][1].encode('iso-8859-1', 'replace')
    nodeset = entries[4][1].encode('iso-8859-1', 'replace')
    case = entries[5][1]
    c = entries[6][1]
    p = entries[7][1]
    v = entries[8][1]
    t = entries[9][1]
    m = entries[10][1]
    CoS = entries[11][1]
    isTool = entries[13][1]

    if(isTool==1):
        print "Tool: 2 Field Outputs"
        FIELDNum=2
    else:
        if(CoS==1):
            print "Continuum Element: 4 Field Outputs"
            FIELDNum=4
        else:
            print "Shell Element: 8 Field Outputs"
            FIELDNum=8
    print "Modifying Input File..."
    try:
        # IMPORTANT STRINGS TO MAKE IT WORK
        mdb.models[Model].keywordBlock.synchVersions()
        #Printing Position and Keyword Block length
        SearchForLine="""*Surface Interaction, name=%s"""%Prop1
        print 'Searching for Keyword "%s"...'%SearchForLine
        position = GetKeywordPosition(Model,SearchForLine)
        print "Found at block",position,"from a total of",len(mdb.models[Model].keywordBlock.sieBlocks)+1,"blocks"
        # ADD YOUR *KEYWORD
        if(isTool==1):
            mdb.models[Model].keywordBlock.replace(position+1, """
*Friction, user=friction, depvar=0, properties=5
%g,%g,%g,%g,%g"""%(c,p,v,t,m))
        else:
            mdb.models[Model].keywordBlock.replace(position+1, """
*Friction, user=friction, depvar=0, properties=6
%d,%g,%g,%g,%g,%g"""%(case,c,p,v,t,m))
            if(CoS==2):
                SearchForLine="""*Surface Interaction, name=%s"""%Prop2
                print 'Searching for Keyword "%s"...'%SearchForLine
                position = GetKeywordPosition(Model,SearchForLine)
                print "Found at block",position,"from a total of",len(mdb.models[Model].keywordBlock.sieBlocks)+1,"blocks"
                mdb.models[Model].keywordBlock.replace(position+1,"""
*Friction, user=friction, depvar=0, properties=6
%d,%g,%g,%g,%g,%g"""%(case,c,p,v,t,m))
        print 'Searching for Keyword "*Bulk Viscosity"...'
        position = GetKeywordPosition(Model,'*Bulk Viscosity')
        x=1
        while(position!=-1):
            print "Found at block",position,"from a total of",len(mdb.models[Model].keywordBlock.sieBlocks)+1,"blocks"
            mdb.models[Model].keywordBlock.insert(position, """
*FIELD, USER, NUMBER=%d
%s"""%(FIELDNum,nodeset))  
            x += 1
            position = GetKeywordPosition(Model,'*Bulk Viscosity',x)
        print 'Keyword "*Bulk Viscosity" was found %d times.'%(x-1)
    except EXCEPTION,e:
        print "Error: ",
        msg.showerror("Error","An unexpected error has occurred while editing the inp. file. Please check all required fields. A possible error could be that no 'Step' exists.")
        print e 
        return False
    else:
        print "Done."
        return True

def CreateFiles(entries):
    JobName = entries[3][1].encode('iso-8859-1', 'replace')
    CPU_usage = entries[12][1]
    print "Creating .inp File..."
    try:
        mdb.jobs[JobName].writeInput(consistencyChecking=OFF)
    except Exception,e:
        print "Error:",
        print e
        msg.showerror("Error","An unknown error has occurred while creating the .inp file. Please check if the job you picked exists.")
        return False
    else:
        print "Done."
    print "Creating .env File..."
    try:
        directory=os.getcwd()+"\\TMP_SCRATCH"
        print "Scratch directory:",directory
        if not os.path.exists(directory):
            os.makedirs(directory)
        directory=repr(directory)
        outToFile = open("abaqus_v6.env", "w")
        outToFile.write("""import os
mp_mode=THREADS
double_precision = BOTH
usub_lib_dir=os.getcwd()
scratch = os.path.join(os.getcwd(),'TMP_SCRATCH')""")
        outToFile.close()
    except Exception,e:
        print "Error:",
        print e
        msg.showerror("Error","An unknown error has occurred while creating the .env file. Please check all required fields.")
        return False
    else:
        print "Done."
    print "Creating Batch script..."
    try:
        outToFile = open("USR_start_%s.bat"%JobName, "w")
        outToFile.write("""abq6141 job=%s input=%s.inp cpus=%d"""%(JobName,JobName,CPU_usage))
        outToFile.close()
    except Exception,e:
        print "Error:",
        print e
        msg.showerror("Error","An unknown error has occurred while creating the batch file. Please check all required fields.")
        return False
    else:
        print "Done."
        return True

def fetch(entries,isProp):
    global entries_out
    blnuseDummy=False
    if(isProp==True):
        print "Identifying properties..."
        #Koeffizienten speichern
        print "case:",USRcase
    else:
        print "Identifying paths..."
    for entry in entries:
        field = entry[0]
        temptext=entry[1].get().strip().replace(',','.')
        temptext=temptext.encode('iso-8859-1', 'replace')
            #Leerstellen mit Dummywerten besetzen
        if(temptext==""):
            if(isProp==True):
                text="0"
                blnuseDummy=True
                print "using dummy-value for",
            else:
                print "Error: No name detected" 
                msg.showerror("Missing Model Assignments", "Please enter valid Model Assignments.") 
                return False
        else:
            if(isProp==True):text=temptext
            else:text=entry[1].get()
        if(field==u'\u03B1'):field='alpha'
        if(field==u'\u03B2'):field='beta'
        if(field==u'\u03B3'):field='gamma'
        if(field==u'\u03B4'):field='delta'
        print('%s: %s' % (field, text.encode('iso-8859-1', 'replace')))
        
        if(isProp==True):
            try:
                text = float(text)
            except ValueError,e:
               msg.showerror("Invalid Entries","Invalid coefficients. Please check your entries.")
               print "Error, invalid properties"
               print e
               return False
               
               break
        entries_out.append((field, text))
    if(blnuseDummy==True):
        msg.showinfo("Using Dummy-variables","""At least one coefficient field was left empty.
Instead, dummy variables will be used for the empty field.""")
    print "Done."
    
    return True
 
def Checkboxchecked():
    global USRcase
    global BlnDelMods
    global lblImage
    a=checkBoxVal1.get()
    b=checkBoxVal2.get()
    c=checkBoxVal3.get()
    d=checkBoxVal4.get()
    e=checkBoxVal5.get()
    if(e==1):
        BlnDelMods=True
    else:
        BlnDelMods=False
    #KSS zur Auswahl stellen/verbergen
    if(a==1 and b==1 and c==1):
        checkBox4.configure(state='active')
    else:
        checkBox4.configure(state='disabled')
        checkBox4.deselect()
        d=0   
    #Fallgebung wird ein Wert zugeordnet
    if (d==1):
        USRcase = 8
    else:
        if(a==1 and b==0 and c==0):
            USRcase = 1
        elif(a==0 and b==1 and c==0):
            USRcase = 2
        elif(a==0 and b==0 and c==1):
            USRcase = 3
        elif(a==1 and b==1 and c==0):
            USRcase = 4
        elif(a==1 and b==0 and c==1):
            USRcase = 5
        elif(a==0 and b==1 and c==1):
            USRcase = 6
        elif(a==1 and b==1 and c==1):
            USRcase = 7
        elif(a==0 and b==0 and c==0):
            USRcase=0
        else:
            msg.showerror("Error", 'An unknown error has occured. Please restart the application.')    
    #der Fall wird angegeben
    if(USRcase==0):
        lblcase_txt.set("")
        btnSubmit.configure(state='disabled')
    else:
        lblcase_txt.set("Fall "+str(USRcase))
        btnSubmit.configure(state='active')
        try:
            formuladir=pluginPath+"\\CASES\\USRcase%d.gif"%USRcase
            formula = PhotoImage(file=formuladir)
            lblImage.config(image=formula, height=30) 
            lblImage.image=formula
        except Exception, e:
            
            lblImage.config(text="Error loading images.", height=30)
    
        
def RadioBtnChoice():
    global TT2ndIntProp
    a=ents1[2][1]
    b=ents1[2][2]
    c=v.get()
    if(c==1):
        a.configure(state='disabled')
        b.configure(state='disabled')
        TT2ndIntProp.configure(state='normal')
    else:
        a.configure(state='readonly')
        b.configure(state='normal')
        TT2ndIntProp.configure(state='disabled')

def cbxupdate(entries,x=0,field=""):
    global currentModel
    if(field==filedir[0]):currentModel=entries[0][1].get()
    a=currentModel
    if(x==-2):
        entries[0][1]["values"]=mdb.models.keys()
        a=mdb.models.keys()[0]
        currentModel=a
    allModels=mdb.models.keys()
    allIntProps=mdb.models[a].interactionProperties.keys()
    allJobs=mdb.jobs.keys()
    tempallSets=mdb.models[a].rootAssembly.allSets.summary()
    allSets = []
    for i in tempallSets:
        if(i[5]=="Node"):#Feste gegebene Stelle für Nodes
            allSets.append(i[0])
    allInstances=mdb.models[a].rootAssembly.instances.keys()
    allKeys=(allModels,allIntProps,allIntProps,allJobs,allSets)
    if(field==filedir2[0]):allKeys=[allSets]
    if(field==filedir3[0]):allKeys=[allJobs]
    if(field=="Instances"):allKeys=[allInstances]
    i=0
    for entry in entries:
        try:entry[1]["values"] = allKeys[i]
        except UnicodeDecodeError, e:
            j=0
            for Key in allKeys[i]:
                Key=unicode(Key,'iso-8859-1')
                
                allKeys[i][j]=Key
                j+=1
            try:entry[1]["values"] = allKeys[i]
            except Exception, e:
                print e
                entry[1].config(state="normal")
        except Exception, e:
            msg.showerror("Error","An unknown error has occured while reading all Model Assignments. Please check all required fields.")
            print e       
        i+=1
    if(x<=0):
        x+=1
        while x<len(entries):
            try:entries[x][1].current(0)
            except Exception,e:
                entries[x][1].set('')
            x+=1
        if(field==filedir[0]):
            cbxupdate(ents11,-1,filedir2[0])
            cbxupdate(ents21,-1,filedir2[0])
            cbxupdate(cbxInst1,-1,"Instances")
            cbxupdate(cbxInst2,-1,"Instances")

def makeDropDown(root,field,values="DEFAULT",size=None):
    entries = []
    row = Frame(root)
    lab = Label(row, text=field+":",anchor='w')
    if(size!=None):
            lab.config(width=size)
    row.pack(side=TOP, fill=X, padx=5, pady=2)
    lab.pack(side=LEFT)
    ent = ttk.Combobox(row, state='readonly')
    ent.pack(side=RIGHT, expand=YES, fill=X)
    ent["values"]=values
    try:ent.current(0)
    except Exception,e:
        ent.set('')
    entries.append((field,ent,lab))
    ent.bind("<<ComboboxSelected>>",(lambda event, field=field:cbxupdate(entries,0,field)))    
    return entries

def CreateSet(newSetName,instances):
    print "Creating Set..."
    
    
    tempName=newSetName.get().strip()
    selectedInstance=instances[0][1].get()
    tempName = tempName.encode('iso-8859-1','replace')
    
    
    if(tempName=="" or tempName[:1]=="_" or tempName[-1:]=="_"):
        msg.showwarning("No name detected","""Please type in a valid name for the node-set.
Names must be 1-38 characters long, may not begin with a number, may not begin or end with an underscore,
and may not contain double quotes, periods, backward slashes, or non printable characters.""")
        print "Aborted: no name detected."
        return
    
    newSetName=newSetName.get().strip().encode('iso-8859-1','replace')
    allSets=mdb.models[currentModel].rootAssembly.allSets.keys()
    
    for ExistingSet in allSets:

        if(newSetName==ExistingSet):
            Areyousure = tkMessageBox.askquestion("Duplicate detected", """A node-set with the given name allready exists.
Do you want to overwrite the existing node-set?""", icon='warning')
            if Areyousure == 'no':
                print "Aborted: Duplicate detected."
                tkMessageBox.showinfo("Aborted","Creating a set has been aborted.")
                return
            else:
                print "Overwriting existing node-set."
    
    
    try:
       
        a = mdb.models[currentModel].rootAssembly
        instance1 = a.instances[selectedInstance.encode('iso-8859-1','replace')].nodes
        a.Set(nodes=instance1, name=newSetName)
    except Exception,e:
        print "Error:",
        print e
        print "Creating Set failed. Please try creating a set manually."
        msg.showerror("Fehler","""An unknown error has occurred. Please check if the name uses valid characters.
Names must be 1-38 characters long, may not begin with a number, may not begin aor end with an underscore,
and may not contain double quotes, periods, backward slashes, or non printable characters.""")
        return
    
    cbxupdate(ents11,0,filedir2[0])
    cbxupdate(ents21,0,filedir2[0])
    print "Done."

def makeform(root,fields,isEntry,size=None,DefEntries=None):
    entries = []
    i=0
    for field in fields:
        row = Frame(root)
        lab = Label(row, text=field+":", anchor='w')
        if(size!=None):
            lab.config(width=size)
        row.pack(side="top", fill=X, padx=5, pady=2)
        lab.pack(side="left")
        if(isEntry==True):ent = Entry(row)
        else:ent = ttk.Combobox(row,state='readonly')
        ent.pack(side="right", expand=YES, fill=X)
        if(DefEntries!=None):
            if(isEntry==True):
                ent.insert(0, DefEntries[i])
            else:
                ent["values"]= DefEntries[i]
                try:ent.current(0)
                except Exception,e:
                    ent.set('')
        entries.append((field,ent,lab))
        if(isEntry!=True):ent.bind("<<ComboboxSelected>>",(lambda event, i=i,field=field:cbxupdate(entries,i,field)))
        i+=1
    return entries
    #Erstellen von Labels und Entries anhand einer Liste
    #Rueckgabe einer Liste mit Zusammenhang Label und Entry

def tabChangedEvent(self):
    global chTab
    global lblImage
    global TT2ndIntProp
    chTab=nb.index(nb.select())
    Checkboxchecked()
    if(chTab==1):
        btnSubmit.configure(state='active')
        formuladir=pluginPath+"\\CASES\\USRcase8.gif"
        try:
            formula = PhotoImage(file=formuladir)
            lblImage.config(image=formula, height=30)
            lblImage.image=formula
        except Exception, e:
            
            lblImage.config(text="Error loading images.", height=30)
    
        a=ents1[2][1]
        b=ents1[2][2]
        a.configure(state='disabled')
        b.configure(state='disabled')
        TT2ndIntProp.configure(state='normal')
    else:
        a=ents1[2][1]
        b=ents1[2][2]
        c=v.get()
        if(c==1):
            a.configure(state='disabled')
            b.configure(state='disabled')
            TT2ndIntProp.configure(state='normal')
        else:
            a.configure(state='readonly')
            b.configure(state='normal')
            TT2ndIntProp.configure(state='disabled')

def submit(e1,e11,e12,e21,e22,e2a,e2b):
    if(BlnDelMods==True):
        Areyousure = tkMessageBox.askquestion("Discarding all edits", """Are you sure discarding all edits?
ALL EDITS AND MODIFICATIONS will be deleted, possibly even those created in past sessions.
Do you want to continue?""", icon='warning')
        if Areyousure == 'yes':
            f2=del_Old_Edits()
            if(f2==False):return
            tkMessageBox.showinfo("Procedure successful","All edits were discarded from the inp. file.")
        return
    global entries_out
    entries_out =[]
    f0=fetch(e1,False)
    if(f0==False):return
    a=chTab
    if(a==0):f01=fetch(e12,False)
    else:f01=fetch(e22,False)
    if(f01==False):return
    if(a==0):f02=fetch(e11,False)
    else:f02=fetch(e21,False)
    if(f02==False):return
    entries_out.append(('case',USRcase))
    if(a==0):f1=fetch(e2a,True)
    else:f1=fetch(e2b,True)
    if(f1==False):return
    entries_out.append(('Continuum(1),Shell(2)',int(v.get())))
    if(a==0):entries_out.append(('CPU Usage',int(use_cpu.get())))
    else:entries_out.append(('CPU Usage',int(use_cpu2.get())))
    entries_out.append(("Workpiece(0),Tool(1):",chTab))
    print entries_out
    f2=del_Old_Edits()
    if(f2==False):return
    f3=USR_Properties(entries_out)
    if(f3==False):return
    f4=ModInpFile(entries_out)
    if(f4==False):return
    f5=CreateFiles(entries_out)
    if(f5==False):return
    print "Procedure complete."
    tkMessageBox.showinfo("Procedure successful","All preferences were loaded and entered.")

    
####################
#Erstellung der Gui#
####################
TT2ndIntProp=""
root=""
app=""
lf0=""
row=""
lf1=""
lf2=""
ents1=""
ents11=""
ents12=""
ents21=""
ents22=""
v=""
lbl_cpu=""
use_cpu=""
use_cpu2=""
checkBoxVal1=""
checkBox1=""
checkBoxVal2=""
checkBox2=""
checkBoxVal3=""
checkBox3=""
checkBoxVal4=""
checkBox4=""
lblcase_txt=""
lblcase=""
ents2a=""
ents2b=""
checkboxVal5=""
btnSubmit=""
btnCancel=""
nb=""
chTab=""
isTool=""
lblImage=""
pluginPath=""

def mainform(plPath=os.getcwd()):
    global TT2ndIntProp,root,nb,app,pluginPath,lf0,lf1,lf2,lfPic,lblImage,row,ents1,ents11,ents12,ents21,ents22,v,use_cpu,lbl_cpu,checkBoxVal1,checkBox1,checkBoxVal2,checkBox2,checkBoxVal3,checkBox3,checkBoxVal4,checkBox4,checkBoxVal5,checkBox5,lblcase_txt,lblcase,ents2a,ents2b,checkboxVal5,btnSubmit,btnCancel
    
    root = Tk()
    root.withdraw()
    app = Toplevel()
    app.title("VFRICTION_USR Wizard v0.11.5")
    app.attributes("-toolwindow",1)
    lblTitle=Label(app,justify="left",text="Welcome to VFRICTION-USR Wizard v.0.11.5!")
    lblTitle.pack()
    lblIntro=Label(app,justify="left",text="""This Abaqus Plug-In was created to modify the selected Abaqus model for the VFRICTION_USR.
Please keep in mind that you are not able to run two simulations simultaniously. Therefore if you
want to simulate both of the options "Workpiece" and "Tool",you will have to run them in sequence. \n
PLEASE NOTE: ABAQUS may freeze in the background while using the plugin. If this happens, just fill
all required fields and complete the plugin run. After closing the plugin window, ABAQUS should 
unfreeze again.\n
Please hover with the cursor over each field to get additional help.""")
    lblIntro.pack(padx=10,pady=5)
    lf0=LabelFrame(app,text="Model Assignments")
    lf0.pack(fill="both", padx=15)
    
    pluginPath=plPath
    
    
    try:
        lfPic=LabelFrame(app, text="Selected Equation for Simulation")
        lfPic.pack(padx=15, pady=3,fill="x")
        lblImage=Label(height=30)
        formuladir=pluginPath+"\\CASES\\USRcase8.gif"
        formula = PhotoImage(file=formuladir)
        lblImage=Label(lfPic, image=formula, height=30)
        lblImage.pack(expand="yes")
    except Exception, e:
        print "Error while loading images: ",
        print e
        lblImage=Label(lfPic, text="Error loading images.", height=30)
    
    nb = ttk.Notebook(app)
    f1 = ttk.Frame(nb)
    f2 = ttk.Frame(nb)
    nb.add(f1, text='Workpiece')
    nb.add(f2, text='Tool')
    nb.bind_all("<<NotebookTabChanged>>", tabChangedEvent)
    nb.pack(fill=X, padx=15, pady=10, expand=YES)
    
    ents1 = makeform(lf0, filedir,False,25,DefNames)
    f1Pref=fillTab(f1,ents11,ents12,ents2a)
    f2Pref=fillTab(f2,ents21,ents22,ents2b,True)
    ents11=f1Pref[0] #Sets
    ents12=f1Pref[1] #Jobs
    ents2a=f1Pref[2] #Coeffs
    ents21=f2Pref[0]
    ents22=f2Pref[1]
    ents2b=f2Pref[2]
    
    checkBoxVal5 = IntVar()
    checkBox5 = Checkbutton(app, variable=checkBoxVal5, text="Discard all edits from the inp. file", command=Checkboxchecked)
    checkBox5.pack()
    
    
    rowBtn=Frame(app)
    rowBtn.pack()
    btnSubmit = Button(rowBtn, text="OK", width=20,command=(lambda e1=ents1,e11=ents11,e12=ents12,e21=ents21,e22=ents22,e2a=ents2a,e2b=ents2b: submit(e1,e11,e12,e21,e22,e2a,e2b)))
    btnSubmit.pack(anchor=S,side='left',padx=15,pady=15)
    btnCancel = Button(rowBtn, text="Cancel", width=20, command=shutdown_ttk_repeat)
    btnCancel.pack(anchor=S,side='left', padx=15,pady=15)
    
    TT2=ToolTip(lfPic,text="The USR will calculate the friction coefficient via shown equation. The constants alpha, beta, gamma, delta and the combined numerical constant c have to be experimentally determined and are required for the simulation. The shown equation changes depending on the chosen criteria.")
    TT3=ToolTip(ents1[0][1],text="The chosen ABAQUS model will be used for the USR simulation.")
    TT4=ToolTip(ents1[1][1],text="The chosen Interaction Property will be used for the USR simulation.")
    TT2ndIntProp=ToolTip(ents1[2][1],text="Continuum Elements require just one Interaction Property.")
    TT2ndIntProp.configure(state='disabled')    
    
    cbxupdate(ents1,-2)
    cbxupdate(ents11,-2,filedir2[0])
    cbxupdate(ents12,-2,filedir3[0])
    cbxupdate(ents21,-2,filedir2[0])
    cbxupdate(ents22,-2,filedir3[0])
    Checkboxchecked()
    RadioBtnChoice()
    app.resizable(width=FALSE, height=FALSE)
    print "Welcome to VFRICTION_USR Wizard v0.11.5 by jk and oa!"
    
    app.protocol('WM_DELETE_WINDOW',shutdown_ttk_repeat)

    app.mainloop()
    app.quit()
    #########TAB1##########
def fillTab(f1,e1,e2,e3,isTool=False):
    global nb,app,lf0,lf1,lf2,row,ents1,ents11,ents12,ents21,ents22,cbxInst1,cbxInst2,v,use_cpu,use_cpu2,lbl_cpu,checkBoxVal1,checkBox1,checkBoxVal2,checkBox2,checkBoxVal3,checkBox3,checkBoxVal4,checkBox4,checkBoxVal5,checkBox5,lblcase_txt,lblcase,ents2,checkboxVal5,btnSubmit,btnCancel
    if(isTool==False):
        row2=Frame(f1)
        row2.pack()
    lf01=LabelFrame(f1,text="Node")
    lf01.pack(fill="both", padx=5)
    lfNodeInfo=Label(lf01,justify="left",text="""The USR will calculate the friction values for the selected part by using the upper equation.
Please choose a Node Set referencing the desired part for calculation.
If you do not have a desired Node Set it is possible to create one with the "Create Nodeset" Tool.""")
    lfNodeInfo.pack(padx=5)
    lf01a=LabelFrame(lf01,text="Create Nodeset")
    lf01a.pack(fill=BOTH,padx=5)
    if(isTool==False):cbxInst1=makeDropDown(lf01a,"Instances",size=20)
    else:cbxInst2=makeDropDown(lf01a,"Instances",size=20)
    rowInst=Frame(lf01a)
    rowInst.pack(anchor="w")
    LblInstances=Label(rowInst,anchor="w",width=20,text="Set Name:")
    LblInstances.pack(side="left",expand="yes",padx=5, pady=2)
    EntInstances=Entry(rowInst)
    EntInstances.pack(side="left",expand="yes",fill=X)
    if(isTool==False):ButtonInstances=Button(rowInst,text="Create",command=(lambda SetName=EntInstances,Inst=cbxInst1:CreateSet(SetName,Inst)))
    else:ButtonInstances=Button(rowInst,text="Create",command=(lambda SetName=EntInstances,Inst=cbxInst2:CreateSet(SetName,Inst)))
    ButtonInstances.pack(side="right",padx=5, pady=2)
    
    lf01b=LabelFrame(lf01,text="Use existing Nodeset")
    lf01b.pack(fill=BOTH,padx=5)
    e1 = makeform(lf01b, filedir2,False,20,DefNames2)
    
    if(isTool==False):cbxupdate(cbxInst1,-2,"Instances")
    else:cbxupdate(cbxInst2,-2,"Instances")
    
    
    row=Frame(f1)
    if(isTool==False):row.pack(fill="x")
    else:row.pack()    
    if(isTool==False):
        v = IntVar()
        v.set(1)
        Radiobutton(row2,text="Continuum",padx = 10,variable=v,value=1,command=RadioBtnChoice).pack(side="left",anchor=S)
        Radiobutton(row2,text="Shell",padx = 10, variable=v,value=2,command=RadioBtnChoice).pack(side="left",anchor=S)
        lf1=LabelFrame(row,text="Formula Criteria")
        lf1.pack(side="left",fill="x",padx=5,pady=2)
    lf2=LabelFrame(row,text="Coefficients")
    lf2.pack(fill="x",side="left",padx=5,pady=2)
    
    lf02=LabelFrame(f1,text="Job")
    lf02.pack(fill="both", padx=5)
    
    e2= makeform(lf02, filedir3,False,20,DefNames3)
    
    if(isTool==False):
        lblCoeff=Label(lf1,text="The calculation will depend on the following criteria.")
        lblCoeff.pack(padx=20)
        rCr=Frame(lf1)
        rCr.pack()
        checkBoxVal1 = IntVar()
        checkBox1 = Checkbutton(rCr, variable=checkBoxVal1, text="P - Contact Pressure [MPa]", command=Checkboxchecked)
        checkBox1.select()
        checkBox1.pack(anchor=W)
        checkBoxVal2 = IntVar()
        checkBox2 = Checkbutton(rCr, variable=checkBoxVal2, text="v - Relative Speed [mm/s]", command=Checkboxchecked)
        checkBox2.pack(anchor=W)
        checkBox2.select()
        checkBoxVal3 = IntVar()
        checkBox3 = Checkbutton(rCr, variable=checkBoxVal3, text="t - Temperature [K]", command=Checkboxchecked)
        checkBox3.pack(anchor=W)
        checkBox3.select()
        checkBoxVal4 = IntVar()
        checkBox4 = Checkbutton(rCr, variable=checkBoxVal4, text="m - Lubricant Amount [g/mm*mm]", command=Checkboxchecked)
        checkBox4.select()
        checkBox4.pack(anchor=W)
        lblcase_txt = StringVar()
        lblcase_txt.set("")
        lblcase = Label(lf1, textvariable=lblcase_txt,fg='dark grey')
    
    lbl_cpu=Label(lf02,text="CPU usage:")
    lbl_cpu.pack(side="left",padx=5)
    if(isTool==False):
        use_cpu = Spinbox(lf02,width=3,from_=1,to_=9999)
        use_cpu.pack(side="left",pady=5)
    else:
        use_cpu2 = Spinbox(lf02,width=3,from_=1,to_=9999)
        use_cpu2.pack(side="left",pady=5)
    lbl_pc_cpu=Label(lf02,text="This PC has %s CPUs."%multiprocessing.cpu_count())
    lbl_pc_cpu.pack(side="left",padx=35)
    
    e3 = makeform(lf2, fields,True,2)
    
    TTCoefficients=ToolTip(lf2, text="The numerical constants are determinded experimentally. If you do not enter a value, empty fields will be set to zero.")
    TTJob=ToolTip(lf02, text="For the safety of your own data please check your available RAM and number of CPUs of the device where you intend to run the simulation.")

    if(isTool==False):
        TTShape=ToolTip(row2,text="Please select the type of the part (continuum or shell) which shall be calculated. You must take the same shape as in your ABAQUS Model!")
    if(isTool==False):TTCreateNode=ToolTip(cbxInst1[0][1],text='Please select the part which shall be used for the USR.')
    else:TTCreateNode=ToolTip(cbxInst2[0][1],text='Please select the part which shall be used for the USR.')
    TTSetName=ToolTip(rowInst, text='After pressing the "Create" Button, the new NodeSet will appear under "Existing Nodesets" below.')
    
    
    
    return (e1,e2,e3)

def _close(self):
    self.quit()

def shutdown_ttk_repeat():
     global app,root
     app.destroy()
     root.destroy()

def __init__(self, parent):
     self.mainroot=parent
     self.mainroot.protocol("WM_DELETE_WINDOW", self.shutdown_ttk_repeat)
    
if __name__=='__main__':mainform()
