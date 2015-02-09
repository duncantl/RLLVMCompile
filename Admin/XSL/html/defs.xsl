<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:r="http://www.r-project.org"
		xmlns:s="http://cm.bell-labs.com/stat/S4"
                xmlns:py="http://www.python.org"
                xmlns:perl="http://www.perl.org"
                xmlns:c="http://www.C.org"
		xmlns:vb="http://www.visualbasic.com"
		xmlns:omegahat="http://www.omegahat.org"
		xmlns:bioc="http://www.bioconductor.org"
                xmlns:java="http://www.java.com"
		xmlns:sql="http://www.sql.org"
                version="1.0">


<xsl:template match="ignore" />

<xsl:template match="squote">
'<xsl:apply-templates/>'
</xsl:template>

<xsl:template match="LaTeX|latex">
 <i>LaTeX</i>
</xsl:template>

<xsl:template match="splus|SPLUS">
 <i>S-Plus</i>
</xsl:template>

<xsl:template match="py:code">
  <xsl:call-template name="code">
    <xsl:with-param name="class">python</xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template match="perl:code|perl:script">
  <xsl:call-template name="code">
    <xsl:with-param name="class">perl</xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template match="java:code">
  <xsl:call-template name="code">
    <xsl:with-param name="class">java</xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template match="vb:code">
  <xsl:call-template name="code">
    <xsl:with-param name="class">VB</xsl:with-param>
  </xsl:call-template>
</xsl:template>


<xsl:template match="sql:code|sql:cmd">
  <xsl:call-template name="code">
    <xsl:with-param name="class">SQL</xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template match="sql:func">
  <xsl:element name="code">
    <xsl:attribute name="class">SQL</xsl:attribute><xsl:value-of select="." />
  </xsl:element>
</xsl:template>

<xsl:template match="invisible"/>


<xsl:template match="VB">
 <i>Visual Basic</i>
</xsl:template>

<xsl:template match="Perl">
 <i>Perl</i>
</xsl:template>

<xsl:template match="Java">
 <i>Java</i>
</xsl:template>

<xsl:template match="Python">
 <i>Python</i>
</xsl:template>

<!--
 If this is here, Rstyle.xsl causes problems in Firefox when coming
 from omegahat.org in the 
       xsl:include href="defs.xsl"
  We have to use xsl:import
  But omitting this allows us to use xsl:include
-->

<xsl:template match="s:code|s:init">			       <!--|r:code|r:script|r:init|s:init -->
  <xsl:call-template name="code">
    <xsl:with-param name="class">S</xsl:with-param>
  </xsl:call-template>
</xsl:template>


<xsl:template match="c:code">
  <xsl:call-template name="code">
    <xsl:with-param name="class">C</xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template match="env">
  <xsl:call-template name="var">
    <xsl:with-param name="class">env</xsl:with-param>
  </xsl:call-template>
</xsl:template>


<xsl:template match="vb:var">
  <xsl:call-template name="var">
    <xsl:with-param name="class">VisualBasic</xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template match="vb:type">
  <xsl:call-template name="type">
    <xsl:with-param name="class">VisualBasic</xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template match="c:type">
  <xsl:call-template name="type">
    <xsl:with-param name="class">C</xsl:with-param>
  </xsl:call-template>
</xsl:template>


<xsl:template match="c:keyword">
  <xsl:call-template name="keyword">
    <xsl:with-param name="class">c</xsl:with-param>
  </xsl:call-template>
</xsl:template>


<xsl:template match="NA|s:NA|s:na">
 <b>NA</b>
</xsl:template>

<xsl:template match="r:na|r:NA">
 <b><a href="library/base/html/NA.html">NA</a></b>
</xsl:template>

<xsl:template match="NaN">
 <b>NaN</b>
</xsl:template>


<xsl:template match="acknowledgments">
 <h2>Acknowledgements</h2>
 <xsl:apply-templates />
</xsl:template>


<xsl:template match="s:field|r:field">
  <i><xsl:apply-templates/></i>
</xsl:template>

<!-- Host name of CRAN repository -->
<xsl:param name="CRAN.host">cran.r-project.org</xsl:param>

<xsl:template match="r:pkg|r:package|s:package|s:library">
 <i>
  <xsl:element name="a">
   <xsl:attribute name="href">http://<xsl:value-of select="$CRAN.host"/>/web/packages/<xsl:value-of select="."/>/index.html</xsl:attribute>
   <xsl:apply-templates/>
  </xsl:element>
  </i>
</xsl:template>


<!-- A package that has a non-standard URL specifies the url for the package via @url.
      But we don't repeat this everywhere. So you only have to specify it once.
      This template takes care of finding the non-degenerate @url for the package and uses that. -->
<xsl:template match="r:pkg[@url]">
  <xsl:param name="pkgName" select="string(.)"/>
 <i>
  <xsl:element name="a">
   <xsl:attribute name="href">
      <xsl:if test="@url=''"><xsl:value-of select="//r:pkg[text() = $pkgName and not(@url = '')][1]/@url"/></xsl:if>
      <xsl:if test="not(@url='')"><xsl:value-of select="@url"/></xsl:if>
   </xsl:attribute>
   <xsl:apply-templates/>
  </xsl:element>
  </i>
</xsl:template>



<xsl:template match="bioc:pkg">
 <i>
  <xsl:element name="a">
   <xsl:attribute name="href">http://www.bioconductor.org/packages/<xsl:value-of select="$bioc.release.number"/>/bioc/html/<xsl:value-of select="."/>.html</xsl:attribute>
   <xsl:apply-templates/>
  </xsl:element>
  </i>
</xsl:template>



<xsl:template match="r:code/r:output|r:test/r:output">
 <xsl:if test="not($runCode) or parent::*/@eval='false'">
   <pre class="routput"><xsl:apply-templates/></pre>
 </xsl:if>
</xsl:template>

<xsl:template match="s:output|r:output" name="routput">
   <pre class="routput"><xsl:apply-templates/></pre>
</xsl:template>

<xsl:template match="ignore" />

<xsl:template name="keyword">
  <b class="${class}"><xsl:apply-templates/></b>
</xsl:template>

<xsl:template name="type">
  <i class="$class"><xsl:apply-templates/></i>
</xsl:template>


<xsl:template match="r:numeric|r:vector|r:list|r:character|r:logical">
  <b class="rkeyword"><xsl:value-of select="local-name()"/></b>
</xsl:template>


<xsl:template match="r:func[@name]|s:func[@name]|s:method[@name]">
  <i>
  <xsl:element name="a">
   <xsl:attribute name="href"><xsl:value-of select="$rhelp.dir"/><xsl:value-of select="@name"/>.html</xsl:attribute>
   <xsl:value-of select="@name"/>()
  </xsl:element>
  </i>
</xsl:template>


<xsl:template match="r:func|s:func|s:method|r:s3method" name="func">
 <i class="rfunc">
<xsl:choose>
<xsl:when test="$add.links.to.rfuncs">
  <xsl:element name="a">
   <xsl:attribute name="href"><xsl:value-of select="$rhelp.dir"/><xsl:value-of select="@pkg"/>/<xsl:value-of select="."/>.html</xsl:attribute>
   <xsl:if test="@pkg"><xsl:attribute name="title"><xsl:value-of select="@pkg"/></xsl:attribute></xsl:if>
  <xsl:call-template name="addTooltip"><xsl:with-param name="name" select="string(.)"/></xsl:call-template>
   <xsl:apply-templates/>()</xsl:element>
</xsl:when>
<xsl:otherwise>
  <xsl:apply-templates/>()
</xsl:otherwise>
</xsl:choose>
  </i>
</xsl:template>




</xsl:stylesheet>
