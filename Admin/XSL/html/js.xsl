<?xml version="1.0" ?>
<xsl:stylesheet 
        xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
        xmlns:xml="http://www.w3.org/XML/1998/namespace"
	xmlns:xp="http://www.w3.org/TR/xpath"
        xmlns:ecma="http://www.ecma-international.org/publications/standards/Ecma-262.htm"
        xmlns:js="http://www.ecma-international.org/publications/standards/Ecma-262.htm"
        xmlns:json="http://www.json.org"
        exclude-result-prefixes="js ecma xp xml xsl json"
        version="1.0">

<xsl:template match="js">
<b class="proglang">JavaScript</b>
</xsl:template>

<xsl:template match="js:code">
 <pre class="javascript"><xsl:apply-templates/></pre>
</xsl:template>

<xsl:template match="js:function">
 <pre class="jsFunctionDef"><xsl:apply-templates/></pre>
</xsl:template>


<xsl:template match="js:func">
 <code class="jsFunc"><xsl:apply-templates/></code>
</xsl:template>

<xsl:template match="js:var">
 <code class="jsVar"><xsl:apply-templates/></code>
</xsl:template>

<xsl:template match="js:expr">
 <code class="jsVar"><xsl:apply-templates/></code>
</xsl:template>

<xsl:template match="js:method">
 <code class="jsVar"><xsl:apply-templates/></code>
</xsl:template>

<xsl:template match="js:param">
 <code class="jsVar"><xsl:apply-templates/></code>
</xsl:template>

<xsl:template match="js:field">
 <code class="jsVar"><xsl:apply-templates/></code>
</xsl:template>

<xsl:template match="js:keyword">
 <code class="jsKeyword"><xsl:apply-templates/></code>
</xsl:template>


<xsl:template match="json:true">
 <code class="jsonKeyword">true</code>
</xsl:template>
<xsl:template match="json:false">
 <code class="jsonKeyword">false</code>
</xsl:template>
<xsl:template match="json:null">
 <code class="jsonKeyword">null</code>
</xsl:template>

</xsl:stylesheet>
