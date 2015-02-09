<?xml version="1.0" ?>
<xsl:stylesheet 
        xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
        xmlns:perl="http://www.perl.org"
        version="1.0">

<xsl:template match="perl:sub">
<font class="perlSub"><xsl:apply-templates/>()</font>
</xsl:template>

<xsl:template match="perl:class">
<code class="perlClass"><xsl:apply-templates/></code>
</xsl:template>

<xsl:template match="perl:cmd">
<code class="per"><xsl:apply-templates/></code>
</xsl:template>

<xsl:template match="perl:variable">
 <b class="perlVariable"><xsl:apply-templates/></b>
</xsl:template>

<xsl:template match="perl:module">
 <b class="perlModule"><xsl:apply-templates/></b>
</xsl:template>


</xsl:stylesheet>
