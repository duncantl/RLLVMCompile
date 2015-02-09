<?xml version="1.0" ?>
<xsl:stylesheet 
        xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
        xmlns:omg="http://www.omegahat.org"
        xmlns:rs="http://www.omegahat.org/RS" 
	xmlns:r="http://www.r-project.org"
        xmlns:s="http://cm.bell-labs.com/stat/S4"
        exclude-result-prefixes="omg r s rs"
        version="1.0">

<!-- 
   Eventhough some of the elements listed here are the same as some 
   in SHelp.xsl in the SHelp package, there is an intended
   difference. In the case of SHelp, we use a different namespace
   for those elements, so they are immediately different and 
   distinguishable. More importantly, the ones in this file
   are aimed at marking up language objects, potentially
   in arbitrary documents and also in serialized versions
   of S-language objects. Those in SHelp are intended to be
   used within the narrower and more specific context of 
   S documentation. There will of course be overlap and
   redundancy that can be merged into a single file as 
   it is identified and settled on.
   -->

<!-- ? -->
<xsl:param name="ver" select="xsl:vendor" />


<xsl:template match="s:package">
 <xsl:element name="a">
  <xsl:attribute name="href">javascript:getPackage(<xsl:if test="@name"><xsl:value-of select="@name"/></xsl:if>
   <xsl:if test="@location">,<xsl:value-of select="@location"/></xsl:if>
   <xsl:if test="@version">,<xsl:value-of select="@version"/></xsl:if>)</xsl:attribute>
  <xsl:if test="node()"><xsl:apply-templates /></xsl:if>
  <xsl:if test="not(node())"><xsl:value-of select="@name" /></xsl:if>
 </xsl:element>
</xsl:template>


<xsl:template name="var">
  <xsl:param name="class">var</xsl:param>
  <b class="${class}">  <xsl:call-template name="addTooltip"><xsl:with-param name="name" select="string(.)"/></xsl:call-template><xsl:apply-templates/></b>
</xsl:template>



<xsl:template match="r:arg|s:arg">
 <i><xsl:apply-templates/></i>
</xsl:template>

<xsl:template match="s:var">
 <b class="svariable"><xsl:apply-templates/></b>
</xsl:template>


  <!-- A url is mapped from
         <url>value</url>
       to 
         <a href="url-value">url-value</a> 
          
       We may want to allow
         <url value="link">text</url>
       which would be mapped to 
         <a href="link">text</a>
    -->
<xsl:template match="s:functionRef|rs:functionRef|s:function" name="rs:functionRef">
  <!-- 
 <xsl:element name="a">
  <xsl:attribute name="href"><xsl:value-of select="normalize-space(.)" />.html</xsl:attribute>
   <xsl:apply-templates/>
 </xsl:element>
 -->
<code class="sfunction"><xsl:apply-templates/>()</code>  
</xsl:template>


<!-- Refers to an operator such as [, [[, +, -, etc. -->
<xsl:template match="s:operator|s:op|r:op|r:operator"> 
 <code class="sfunction"><xsl:apply-templates/></code>
</xsl:template>

<!-- See the version in SHelp.xsl.
     One of these should call the other to consolidate code. -->
<xsl:template match="s:functionRef[@pkg]">
<code>
 <xsl:element name="a">
  <xsl:attribute name="href">
   <xsl:choose>
     <xsl:when test="@pkg='.'">file:///tmp/R/pkg/XML/html/<xsl:value-of select="normalize-space(.)" />.html</xsl:when>
     <!-- Want to get the value of the parameter whose name is identified by @pkg 
          Perhaps time for Javascript! -->
     <xsl:when test="not(@pkg='.')">javascript:getRHelp('<xsl:choose><xsl:when test="@name"><xsl:value-of select="@name"/></xsl:when><xsl:when test="not(@name)"><xsl:value-of select="normalize-space(.)" /></xsl:when></xsl:choose>.html','<xsl:value-of select="normalize-space(@pkg)"/>')</xsl:when>
   </xsl:choose>
  </xsl:attribute>
   <xsl:apply-templates/>
 </xsl:element>
</code>  
</xsl:template>


<xsl:template match="s:classRef">
<xsl:element name="a">
 <xsl:attribute name="href">
  <xsl:value-of select="string(.)" />
 </xsl:attribute>
 <xsl:apply-templates />
</xsl:element>
</xsl:template>

<xsl:template match="s:slot"> 
<code class="Sslot"><xsl:apply-templates /></code>
</xsl:template>



<!-- The logical literals and NULL.
     The user specifies these with no content such as
     <s:null>, <s:true>, <s:false>
  -->
<xsl:template match="s:null|r:null"> 
  <i><code>NULL</code></i>
</xsl:template>

<xsl:template match="s:true|s:TRUE|r:TRUE|r:true"> 
  <i><code>TRUE</code></i>
</xsl:template>
<xsl:template match="s:false|s:FALSE|r:false|r:FALSE"> 
  <i><code>FALSE</code></i>
</xsl:template>

<!-- Here we leave the formatting to the HTML Cascading Style Sheet -->
<!-- (CSS) rather than doing it here. This gives greater flexibility -->
<!-- at the potential expense of complexity and distribution of concepts. -->
<xsl:template match="s:class"> 
  <i class="sclass"><xsl:apply-templates /></i>
</xsl:template>


<!-- A variable name or symbol in the language. -->
<xsl:template match="rs:name|s:variable">
 <b><xsl:apply-templates /></b>
</xsl:template>


<xsl:template match="s:dots|r:dots">
 <b>...</b>
</xsl:template>


<!-- The name of a parameter in a function -->
<xsl:template match="rs:arg">
<code class="argName">
 <xsl:element name="a">
 <xsl:attribute name="href">#<xsl:value-of select="normalize-space(text())" /></xsl:attribute>
  <xsl:value-of select="normalize-space(text())" />
 </xsl:element>
 </code>
  <xsl:apply-templates select="defaultValue" /> 
</xsl:template>

<!-- Default value for a function parameter -->
<xsl:template match="s:defaultValue">
=<code><xsl:apply-templates/></code>  
</xsl:template>


<!-- See SHelp.xsl in the SHelp package for a more complete version.  -->
<xsl:template match="s:argRef">
<code class="argName">
 <xsl:element name="a">
 <xsl:attribute name="href">#<xsl:value-of select="normalize-space(text())" /></xsl:attribute>
  <xsl:value-of select="normalize-space(text())" />
 </xsl:element>
 </code>
</xsl:template>

<!-- This is a block of code, rather than an HTML code element. -->
<xsl:template match="s:code">
 <pre class="Scode">
  <xsl:apply-templates />
 </pre>
</xsl:template>

<xsl:template match="s:commands">
 <pre class="Scommand">
  <xsl:apply-templates />
 </pre>
</xsl:template>


<xsl:template match="s:expression|s:expr|r:expr">
 <code class="Sexpression">
  <xsl:apply-templates />
 </code>
</xsl:template>

<xsl:template match="r:formula">
 <code class="formula">
  <xsl:apply-templates />
 </code>
</xsl:template>



<xsl:template match="rs:keyword|s:keyword|r:keyword">
 <b class="rkeyword"><xsl:apply-templates /></b>
</xsl:template>

<!-- We want to avoid expanding quotes, etc. here. -->
<xsl:template match="s:output">
<pre class="soutput">
  <xsl:apply-templates />
</pre>
</xsl:template>


<xsl:template match="s:plot|r:plot">
 <pre class="scode">
               <xsl:attribute name="title">R plot<xsl:if test="@id"> ID: <xsl:value-of select="@id"/></xsl:if></xsl:attribute>
   <xsl:apply-templates />
 </pre>
<p/>
<xsl:if test="@img and not(@img='')"> 
<xsl:message>Making plot</xsl:message>
 <xsl:element name="img">
  <xsl:attribute name="src"><xsl:value-of select="@img"/></xsl:attribute>
 </xsl:element>
</xsl:if>
</xsl:template>

</xsl:stylesheet>
