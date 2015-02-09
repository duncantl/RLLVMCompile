<?xml version="1.0" ?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:omg="http://www.omegahat.org"
		xmlns:rwx="http://www.omegahat.org/RwxWidgets"
                exclude-result-prefixes="omg rwx"
                version="1.0">

<xsl:template match="omg:func[@pkg]">
 <i>
  <xsl:element name="a">
    <xsl:attribute name="href">http://www.omegahat.org/<xsl:value-of select="@pkg"/>/<xsl:value-of select="."/>.html</xsl:attribute>
   <xsl:apply-templates/>()
  </xsl:element>
 </i>
</xsl:template>

<xsl:template match="rwx:func">
 <i>
  <xsl:element name="a">
    <xsl:attribute name="href">http://www.omegahat.org/RwxWidgets/<xsl:value-of select="@pkg"/>/<xsl:value-of select="."/>.html</xsl:attribute>
   <xsl:apply-templates/>()
  </xsl:element>
 </i>
</xsl:template>


<xsl:template match="omg:func">
 <i><xsl:value-of select="."/>()</i>
</xsl:template>

<xsl:template match="omg:pkg">
  <a><xsl:attribute name="href">
   http://www.omegahat.org/<xsl:choose><xsl:when test="string(.) = 'XML'">RSXML</xsl:when><xsl:otherwise><xsl:value-of select="string(.)"/></xsl:otherwise></xsl:choose>
  </xsl:attribute>
   <xsl:apply-templates />
  </a>
</xsl:template>


</xsl:stylesheet>



