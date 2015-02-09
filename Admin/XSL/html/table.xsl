<?xml version="1.0"?>

<!-- Copyright the Omegahat Project for Statistical Computing, 2000 -->
<!-- Author: Duncan Temple Lang -->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
                version="1.0">


<!-- Let't these pass through unaltered. This is broken. Can't work. Doesn't get called. -->
<xsl:template match="table|th|td">
 <xsl:element name="th"> <!-- string(name(.))  is now causing problems. -->
   <xsl:apply-templates />
 </xsl:element>
</xsl:template>

<xsl:template match="tr">
 <xsl:element name="tr">
  <xsl:attribute name="align">left</xsl:attribute>
  <xsl:apply-templates />
 </xsl:element>
</xsl:template>

</xsl:stylesheet>