from abaqusGui import getAFXApp, Activator, AFXMode
from abaqusConstants import ALL
import os
thisPath = os.path.abspath(__file__)
thisDir = os.path.dirname(thisPath)

toolset = getAFXApp().getAFXMainWindow().getPluginToolset()
toolset.registerGuiMenuButton(
    buttonText='WZL|HSMC-GUI', 
    object=Activator(os.path.join(thisDir, 'hSMCDB.py')),
    kernelInitString='',
    messageId=AFXMode.ID_ACTIVATE,
    #icon='iconPlotMat24x24.bmp',
    applicableModules=ALL,
    version='0.5.130405',
    author='Daniel Trauth, WZL der RWTH Aachen University',
    description='N/A',
    helpUrl='N/A'
)
