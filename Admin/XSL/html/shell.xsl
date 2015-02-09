<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:r="http://www.r-project.org"
		xmlns:s="http://cm.bell-labs.com/stat/S4"
		xmlns:c="http://www.C.org"
                xmlns:python="http://www.python.org"
                xmlns:perl="http://www.perl.org"
		xmlns:vb="http://www.visualbasic.com"
		xmlns:omegahat="http://www.omegahat.org"
                xmlns:bioc="http://www.bioconductor.org"
                xmlns:java="http://www.java.com"
		xmlns:statdocs="http://www.statdocs.org"
		xmlns:gtk="http://www.gtk.org"
		xmlns:com="http://www.microsoft.com"
		xmlns:sh="http://www.shell.org"
                version="1.0">


<xsl:template match="sh:command">
  <code class="shell"><xsl:apply-templates /></code>
</xsl:template>


<xsl:template match="sh:output">
 <!-- <xsl:call-template name="inline.monoseq"/>/ -->
 <pre class="shellOutput">
 <xsl:apply-templates />
 </pre>
</xsl:template>

<xsl:template match="sh:script">
  <xsl:call-template name="inline.monoseq"/>/
</xsl:template>

<xsl:template match="sh:env">
$<xsl:apply-templates />
</xsl:template>

<xsl:template match="dir">
 <code class="dir"><xsl:apply-templates />/</code>
</xsl:template>

<xsl:template match="file">
 <code class="file"><xsl:apply-templates /></code>
</xsl:template>

<xsl:template match="executable|sh:exec">
 <code class="executable"><xsl:apply-templates /></code>
</xsl:template>


<xsl:template match="sh:arg">
 <code class="shellArg"><xsl:apply-templates /></code>
</xsl:template>


<xsl:template match="sh:var">
 <code class="shellVar"><xsl:apply-templates /></code>
</xsl:template>


<xsl:template match="sh:cmd|sh:expr">
 <code class="shellCommand"><xsl:apply-templates /></code>
</xsl:template>

<xsl:template match="sh:code" name="displayShellCode">
<pre class="shell">
 <xsl:apply-templates />
</pre>
</xsl:template>

<xsl:template match="sh:codex">
  <xsl:call-template name="code">
    <xsl:with-param name="class">shell</xsl:with-param>
  </xsl:call-template>
</xsl:template>

</xsl:stylesheet>
