library(XML)
library(readr)


# XML STRING 
prefix.xml <- "sampleItem.xml"
	
read_file(prefix.xml)

# BUILD XML TREE
doc = xmlTreeParse(prefix.xml, useInternalNodes = T)     # PARSE STRING
root = xmlRoot(doc)                                      # FIND ROOT

# DEFINE ITEM
itemNode = newXMLNode("item", parent=root)           # ADD TO ROOT
newXMLNode('name','Greater Sword of XML', parent=itemNode) #ADD
newXMLNode('type','M', parent=itemNode)
newXMLNode('magic','1', parent=itemNode)
newXMLNode('weight','0', parent=itemNode)
newXMLNode('dmg1','40d4', parent=itemNode)
newXMLNode('dmgtype','S', parent=itemNode)
newXMLNode('property','2G', parent=itemNode)
newXMLNode('rarity','Legendary', parent=itemNode)
newXMLNode('text','This great and shiny sword was added automaticaly to your backpack using an R script, and soon a Shiny App', parent=itemNode)

# VIEW XML
print(doc)

# New Item have been added to the root ! Now, just import to your GM app.
