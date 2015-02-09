<?xml version="1.0" ?>
<xsl:stylesheet 
        xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
        xmlns:xml="http://www.w3.org/XML/1998/namespace"
	xmlns:xp="http://www.w3.org/TR/xpath"		       
	xmlns:kml="http://earth.google.com/kml/2.1"
        exclude-result-prefixes="xml xp kml"
        version="1.0">
<!-- xpath in separate file ?-->

<xsl:template match="xml">
<b class="proglang">XML</b>
</xsl:template>

<xsl:template match="xml:tag|xml:element|xml:node|xml:tagName">
 <code class="xmlTag">&lt;<xsl:apply-templates/>&gt;</code>
</xsl:template>

<xsl:template match="xml:attribute|xml:attr">
 <code class="xmlAttribute"><xsl:apply-templates/></code>
</xsl:template>

<xsl:template match="xml:namespace">
<code class="xmlNamespace"><xsl:apply-templates/></code>
</xsl:template>


<xsl:template match="xp:expr">
 <font class="xpath"><xsl:apply-templates /></font>
</xsl:template>


<xsl:template match="xml:expr">
 <code class="xml"><xsl:apply-templates /></code>
</xsl:template>

<xsl:template match="xml:code|xml:inline-code">
 <pre class="xml"><xsl:apply-templates /></pre>
</xsl:template>

<xsl:template match="xp:func">
 <code class="xpath-func"><xsl:apply-templates />()</code>
</xsl:template>

<xsl:template match="xp:code">
 <pre class="xpath"><xsl:apply-templates /></pre>
</xsl:template>


<xsl:template match="xp:call">
 <b class="xpathcall"><xsl:apply-templates /></b>
</xsl:template>

<xsl:template match="xsl:func">
 <b class="xslfunc"><xsl:apply-templates /></b>
</xsl:template>


<xsl:template match="xp:attr">
 <b class="xpathattr"><xsl:apply-templates /></b>
</xsl:template>


<xsl:template match="xsl:call">
 <b class="xslcall"><xsl:apply-templates /></b>
</xsl:template>

<xsl:template match="xml:xmlns">
 <b class="xmlns"><xsl:apply-templates /></b>
</xsl:template>

<xsl:template match="kml:style"><code class="kmlStyle"><xsl:apply-templates/></code></xsl:template>


<xsl:template match="xsl:param | xsl:var">
 <b class="xsl-param"><xsl:if test="@doc"><xsl:attribute name="title"><xsl:value-of select="@doc"/></xsl:attribute></xsl:if>
   <xsl:apply-templates/>
 </b> 
</xsl:template>

</xsl:stylesheet>
