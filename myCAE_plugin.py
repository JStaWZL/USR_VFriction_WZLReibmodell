from abaqusGui import getAFXApp, FXXPMIcon
import os
import i18n

absPath = os.path.abspath(__file__)
absDir  = os.path.dirname(absPath)
helpUrl = os.path.join(absDir, 'helpfile.txt')

toolset = getAFXApp().getAFXMainWindow().getPluginToolset()


# Register a kernel plug-in in the Plug-ins menu.
#
toolset.registerKernelMenuButton(
    moduleName='GUIPlugin', functionName='mainform(%s)'%repr(os.getcwd()),
    buttonText=i18n.tr('User Subroutine|USR VFRICTION Wizard'),
    version='0.11.5', author='jk and oa',
    description='A simple Wizard for configuring an Abaqus model' \
                ' for the modified user subroutine VFRICTION.\n\n' \
                'This plug-in will set all required coefficients.' \
                ' Because this plug-in uses Tkinter, Abaqus may ' \
                'freeze while the window is opened. Abaqus should'\
                ' restore its original state after using and closing the plugin.',
    helpUrl=helpUrl
)