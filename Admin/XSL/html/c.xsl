<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
                xmlns:c="http://www.C.org"
                xmlns:cpp="http://www.cplusplus.org"
                version="1.0">

<xsl:template match="c:struct">
 <b class="cstruct"><xsl:apply-templates/></b>
</xsl:template>

<xsl:template match="c:func|c:routine|c:method|c:field|c:type|c:enumValue">
 <i><xsl:attribute name='class'>c<xsl:value-of select="local-name()"/></xsl:attribute><xsl:apply-templates/></i>
</xsl:template>

<xsl:template match="c:variable|c:var">
 <i class="cvariable"><xsl:apply-templates/></i>
</xsl:template>

<xsl:template match="c:keyword">
 <b class="ckeyword"><xsl:apply-templates/></b>
</xsl:template>

<xsl:template match="c:null"><b class="cnull">NULL</b></xsl:template>

<xsl:template match="c:expr|cpp:expr">
 <code class="C"><xsl:apply-templates/></code>
</xsl:template>

<xsl:template match="cpp:code">
  <xsl:call-template name="code">
    <xsl:with-param name="class">cpp</xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template match="c:code">
  <xsl:call-template name="code">
    <xsl:with-param name="class">C</xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template match="c:routineDef">
  <xsl:call-template name="code">
    <xsl:with-param name="class">Croutine</xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template match="c:arg|c:param">
  <xsl:call-template name="code">
    <xsl:with-param name="class">Carg</xsl:with-param>
  </xsl:call-template>
</xsl:template>

</xsl:stylesheet>
