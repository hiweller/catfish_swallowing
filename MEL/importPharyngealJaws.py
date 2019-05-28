import maya.cmds as cmds
import glob

# Import an individual OBJ file
def importFile(fileName, groupName):
    cmds.file(fileName, i=True, groupReference=True, groupName=groupName)

# Move and rotate an object
def moveFile(objName, tX=0, tY=0, tZ=0, rX=0, rY=0, rZ=0):
    cmds.select(objName)
    cmds.move(tX, tY, tZ)
    cmds.rotate(rX, rY, rZ)

# Parent one object to another
def parentFile(childObj, parentObj):
    cmds.parent(childObj, parentObj, relative=True)

# Set prey bead scale to 1.5
def setPrey():
    cmds.select("prey")
    cmds.scale(1.5, 1.5, 1.5)
    
def importPharyngealJaws(catID):
    
    # Define project directory and OBJ directory
    projectDir = "/Users/hannah/Dropbox/Brainerd_Lab/Projects/Catfish_swallowing/"
    objDir = "Data/Bone_OBJs"
    
    # Build out file name for pharyngeal jaws OBJ
    fileName = projectDir + objDir + "/Cat " + catID + " PharyngealJaws.obj"
    
    # Import file
    importFile(fileName, "PharyngealJaws")
    
    # Parent jaws to neurocranium
    parentFile("PharyngealJaws", "Neurocranium:Mesh")
    
    
def importOperculum(catID):
    # Define project directory and OBJ directory
    projectDir = "/Users/hannah/Dropbox/Brainerd_Lab/Projects/Catfish_swallowing/"
    objDir = "Data/Bone_OBJs"
    
    # Build out file name for pharyngeal jaws OBJ
    fileName = projectDir + objDir + "/Cat " + catID + " OperculumL.obj"
    

    # Import file
    importFile(fileName, "Operculum")
