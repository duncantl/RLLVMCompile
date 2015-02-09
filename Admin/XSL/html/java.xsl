<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"  
                xmlns:java="http://www.java.com"  
                version="1.0">

<xsl:template match="java:method">
 <font class="javaMethod"><xsl:apply-templates />()</font>
</xsl:template>


<xsl:template match="java:class">
 <font class="javaClass"><xsl:apply-templates /></font>
</xsl:template>

<xsl:template match="java:code">
  <xsl:call-template name="code">
    <xsl:with-param name="class">Java</xsl:with-param>
  </xsl:call-template>
</xsl:template>

</xsl:stylesheet>
