<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
		exclude-result-prefixes="r"
                version='1.0'
		xmlns:r="http://www.r-project.org">

<xsl:import href="http://www.omegahat.org/XSL/docbook/expandDB.xsl"/>

<xsl:template match="r:function">
  <xsl:copy>
   <xsl:apply-templates />
  </xsl:copy>
</xsl:template>


<xsl:template match="r:code[@ref]">
 <xsl:variable name="refId"><xsl:value-of select="@ref"/></xsl:variable>
 <xsl:message>reference <xsl:value-of select="$refId"/></xsl:message>
 <xsl:apply-templates select="//r:code[@id = $refId]/* | //r:code[@id = $refId]/text()"/>
</xsl:template>



<xsl:template match="graphic[not(@width) and not(@height)]">
<xsl:copy>
  <xsl:attribute name="width">5.9in</xsl:attribute>
  <xsl:apply-templates select="@* | node()"/>
</xsl:copy>
</xsl:template>


</xsl:stylesheet>