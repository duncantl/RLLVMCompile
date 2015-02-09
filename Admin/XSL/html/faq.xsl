<?xml version="1.0"?>

<!-- Copyright the Omegahat Project for Statistical Computing, 2000 -->
<!-- Author: Duncan Temple Lang -->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
                xmlns:bib="http://www.bibliography.org"
                xmlns:c="http://www.C.org"
                xmlns:rs="http://www.omegahat.org/RS"
                xmlns:r="http://www.r-project.org"
                xmlns:s="http://cm.bell-labs.com/stat/S4"
                exclude-result-prefixes="bib rs s c"
                version="1.0">

<xsl:output method="html" />
<xsl:param name="doIncludes" select="'true'" />
<xsl:param name="cssFile" select="'faq.css'" />

<xsl:include href="http://docbook.sourceforge.net/release/xsl/current/html/docbook.xsl" /> 
<xsl:include href="http://www.omegahat.org/XSL/html/Rsource.xsl" />
<xsl:include href="http://www.omegahat.org/XSL/html/c.xsl" />
<xsl:include href="http://www.omegahat.org/XSL/html/defs.xsl" />
<xsl:include href="http://www.omegahat.org/XSL/html/SLanguage.xsl" />
<xsl:include href="http://www.omegahat.org/XSL/html/Rstyle.xsl" />

<xsl:template match="/">
<xsl:apply-templates match="faq"/>
</xsl:template>

<xsl:template match="p"><xsl:copy/></xsl:template>

<xsl:template match="/FAQ|/faq">
 <HTML>
  <HEAD>
  <xsl:element name="link">
    <xsl:attribute name="rel">stylesheet</xsl:attribute>
    <xsl:attribute name="style">text/css</xsl:attribute>
    <xsl:attribute name="href"><xsl:value-of select="$cssFile"/></xsl:attribute>
  </xsl:element>
  <title>FAQ</title>
  </HEAD>
  <BODY>
  <xsl:apply-templates select="./title" />
  <dl>
    <xsl:apply-templates select="/faq/q | /faq/question | /faq/note" />   <!--everything but the /faq/title. -->
  </dl>
  </BODY>
 </HTML>
</xsl:template>

<xsl:template match="faq/title">
 <h2><xsl:copy-of select="." /></h2>
</xsl:template>

<xsl:template match="question | q">
 <dt class="question">
   Q. <xsl:value-of select="position()"/> <xsl:apply-templates />   
 </dt>
   <center><hr width="5%" /></center>
</xsl:template>


<xsl:template match="answer | a">
 <dd class="answer">
   A. <xsl:apply-templates />   
 </dd>
</xsl:template>


</xsl:stylesheet>
