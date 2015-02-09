<?xml version="1.0"?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
                version="1.0">

<xsl:template match="filename">
 <xsl:element name="a">
  <xsl:attribute name="href">javascript:loadFile("<xsl:if test="@name"><xsl:value-of select="@name"/></xsl:if><xsl:if test="not(@name)"><xsl:value-of select="normalize-space(.)"/>")</xsl:if></xsl:attribute>
 <i class="filename">
    <xsl:if test="node()"><xsl:value-of select="."/></xsl:if>
    <xsl:if test="not(node())"><xsl:value-of select="@name"/></xsl:if>
 </i>
 </xsl:element>
</xsl:template>

<xsl:template match="dll">
<i><xsl:apply-templates /></i>
</xsl:template>

</xsl:stylesheet>
