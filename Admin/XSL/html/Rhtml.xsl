<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
      xmlns:svg="http://www.w3.org/2000/svg"
      xmlns:html="http://www.w3.org/TR/html401"
      xmlns:ltx="http://www.latex.org"
      xmlns:mml="http://www.w3.org/1998/Math/MathML"
   xmlns:xd="http://www.xsldoc.org"
      exclude-result-prefixes="svg xsl html ltx mml r"
      xmlns:r="http://www.r-project.org"
      version="1.0">

  <!-- include or import? -->
<xsl:import href="http://docbook.sourceforge.net/release/xsl/current/html/docbook.xsl" />
<xsl:import href="Rstyle.xsl" />

<xsl:import href="../common/RCommonDocbook.xsl" />
<xsl:import href="../common/no-latex.xsl" />
<xsl:import href="../common/no-fo.xsl" />

<xd:doc what="nothing">
This is an example of some documentation for an XSL thing.
</xd:doc>

<xsl:template match="pseudocode">
    <code class="pseudocode"><xsl:apply-templates/></code>
</xsl:template>

<xsl:template match="rdb">
<a><xsl:attribute name="href"><xsl:value-of select="."/>.html</xsl:attribute><xsl:value-of select="."/></a>
</xsl:template>


<xsl:template match="svg:svg">
<embed type="image/svg+xml"> <xsl:copy-of select="@*"/> </embed>
</xsl:template>

<xsl:template match="html:literal">
 <xsl:copy-of select="."/>
</xsl:template>

<xsl:template match="tex"/>
 

<xsl:template match="proglang">
<b class="proglang"><xsl:apply-templates/></b>
</xsl:template>


<!-- Take over the definition of acronym.  Acronym can have a def attribute and we will add that.
     Alternatively you can use acronymDef with a value attribute and then free form content 
     Use this if the text for the definition is complex and cannot be an attribute. -->
<xsl:template match="acronym">
<xsl:param name="val" select="string(.)"/>
<xsl:param name="def"><xsl:choose><xsl:when test="//acronymDef[@value = $val]"><xsl:value-of select="//acronymDef[@value = $val]"/></xsl:when><xsl:otherwise><xsl:value-of select="//acronym[@def and string(.) = $val]/@def"/></xsl:otherwise></xsl:choose></xsl:param>
<b class="acronym"><xsl:attribute name="title"><xsl:value-of select="string($def)"/></xsl:attribute><xsl:apply-templates/></b>
<xsl:if test="@def"><xsl:text> </xsl:text>(<xsl:value-of select="@def"/>)</xsl:if>
</xsl:template>


<xsl:template match="makeGlossary">
<xsl:if test="not(ancestor::glossary)">
<h2 class="glossary">Glossary</h2>
</xsl:if>
<table class="glossaryTable">
<tr><th>Acronym</th><th>Definition</th></tr>
<xsl:apply-templates select="//acronymDef | //acronym[@def]" mode="glossary"/>
</table>
</xsl:template>

<xsl:template match="acronymDef" mode="glossary">
<tr><td><xsl:value-of select="@value"/></td>
    <td><xsl:apply-templates/></td></tr>
</xsl:template>

<xsl:template match="acronym[@def]" mode="glossary">
<tr><td><xsl:apply-templates /></td>
    <td><xsl:value-of select="@def"/></td></tr>
</xsl:template>


<xsl:template match="html"><b class="progLang">HTML</b></xsl:template>

<!-- math in HTML via MathJAX-->
<xsl:template match="ltx:eqn[starts-with(normalize-space(string(.)), '\[')]"><xsl:apply-templates/></xsl:template>
<xsl:template match="ltx:eqn">\(<xsl:apply-templates/>\)</xsl:template>

<xsl:template match="mml:eqn[./math]"><xsl:copy-of select="./*"/></xsl:template>
<xsl:template match="mml:eqn"><math display="block"><xsl:copy-of select="./*"/></math></xsl:template>

<xsl:template match="linkTable" name="makeLinkTable">
<xsl:if test="not(ancestor::section)">
 <h2>List of Links</h2>
</xsl:if>
<ul>
<xsl:apply-templates select="//ulink[@url]" mode="link-table"/>
</ul>
</xsl:template>

<xsl:template match="ulink" mode="link-table">
 <li>
   <a><xsl:attribute name="href"><xsl:value-of select="@url"/></xsl:attribute><xsl:choose><xsl:when test="count(./*)"><xsl:apply-templates/></xsl:when><xsl:otherwise><xsl:value-of select="@url"/></xsl:otherwise></xsl:choose></a>
 </li>
</xsl:template>

<xsl:template match="r:help">?<b class="rhelp"><xsl:apply-templates/></b></xsl:template>


<xsl:template match="ltx:call"><b class="latex-call"><xsl:apply-templates/></b></xsl:template>

<xsl:template match="fix"/>
<xsl:template match="check"/>
<xsl:template match="r|R">R</xsl:template>

<xsl:template match="title/subtitle"><br/><xsl:apply-templates/></xsl:template>

</xsl:stylesheet>

