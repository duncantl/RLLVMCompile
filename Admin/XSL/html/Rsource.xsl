<?xml version="1.0"?>

<!-- Copyright the Omegahat Project for Statistical Computing, 2000 -->
<!-- Author: Duncan Temple Lang -->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
                xmlns:bib="http://www.bibliography.org"
                xmlns:c="http://www.C.org"
                xmlns:rs="http://www.omegahat.org/RS"
                xmlns:s="http://cm.bell-labs.com/stat/S4"
                xmlns:r="http://www.r-project.org"
                xmlns:omegahat="http://www.omegahat.org"
		xmlns:docbook="http://docbook.org/ns/docbook"
                extension-element-prefixes="r"
		exclude-result-prefixes="s r c rs bib omegahat docbook"
                version="1.0">

<!--                omit-result-prefixes="yes" -->



<xsl:output method="html"  
            encoding="ISO-8859-1"
            indent="yes"/>

<!-- controls whether we put links on r:code elements which 
     to other r:code elements that refer to them, i.e. r:code ref="@id".
    -->
<xsl:param name="add-link-to-refs" select="0" />

<xsl:param name="add-toggle-code-controls" select="$runCode" />

<xsl:param name="toggle.hidden.js">http://www.omegahat.org/DynDocs/JavaScript/toggleHidden.js</xsl:param>



<xsl:template name="make-id-list">
  <!-- Create a list with entries for each of the nodes with an id 
       and make them links to be able to jump to that "transformed" node. -->
<xsl:if test="$suppress.navigation = '0'">
 <div id="navigation">
   <ul class="list">
     <xsl:apply-templates select="//r:code[@id]|//r:init[@id]|//r:function[@id]" mode="toc"/>
   </ul>
  </div>
</xsl:if>
</xsl:template>




<!-- <xsl:template match="text()" mode="reference"><xsl:copy /></xsl:template> -->



<xsl:template match="r:code/r:code[@ref]|r:plot/r:code[@ref]|r:init/r:code[@ref]|r:function/r:code[@ref]|r:code/r:frag[@ref]|r:plot/r:frag[@ref]|r:init/r:frag[@ref]|r:function/r:frag[@ref]">
<xsl:variable name="ref" select="@ref"/>
<xsl:choose>
 <xsl:when test="$r-inline-code-refs">
   <!-- Put tooltip here to show the origin. -->
   <!-- Rather than applying the regular template we might want to just copy the text.
        But we have to handle the case of recursive references. So use a mode. -->
  <xsl:apply-templates select="//r:code[@id = $ref]" mode="reference"/>
 </xsl:when>
 <xsl:otherwise>
  &lt;&lt;<a class="r-code-chunk-ref"><xsl:attribute name="href">#<xsl:value-of select="@ref"/></xsl:attribute><xsl:value-of select="@ref"/></a>&gt;&gt;
 </xsl:otherwise>
</xsl:choose>
</xsl:template>

<xsl:template match="r:option|r:opt">
  <b class="roption">
    <xsl:apply-templates />  
  </b>
</xsl:template>


<!-- 'http://www.omegahat.org/XDynDocs/icons' -->
<xsl:param name="r-xsl-icon-dir"><xsl:value-of select="$admon.graphics.path"/></xsl:param>
<xsl:param name="icon-image-extension"><xsl:value-of select="$admon.graphics.extension"/></xsl:param>

<xsl:template match="incorrect">
<xsl:message>incorrect</xsl:message>
<div class="incorrect">
<div class="admonImage"><img align="left"><xsl:attribute name="src"><xsl:value-of select="$r-xsl-icon-dir"/>caution<xsl:value-of select="$icon-image-extension"/></xsl:attribute></img></div>
<xsl:apply-templates/>
</div>
</xsl:template>

<xsl:template name="generateToggleAllCodeJS">
<xsl:text>&#010;</xsl:text>
<script type="text/javascript"><xsl:comment>
var toggleCodeIds = [
 <xsl:for-each select="//r:plot|//r:code|//r:init|//r:frag|//r:load|//r:test|//r:commands">
   "<xsl:value-of select="generate-id(.)"/>"<xsl:if test="not(position() = last())">, </xsl:if>
 </xsl:for-each>
];
</xsl:comment>
</script>

<p>
<xsl:if test="$add-toggle-code-controls">
<div class="toggleControls">
<a class="toggleAll" onclick="toggleAll(toggleCodeIds)" title="toggle display of all code chunks">+</a>
</div>
</xsl:if>
</p>

<xsl:text>&#010;</xsl:text>
</xsl:template>

<xsl:template match="r:code[@showCode='false']|r:init[@showCode='false']|r:commands[@showCode='false']|r:test[@showCode='false']">
 <div><xsl:attribute name='title'><xsl:call-template name="formatRCode"/></xsl:attribute>
   <xsl:apply-templates select="r:output"/>
 </div>
</xsl:template>




<xsl:template match="r:code|r:init|r:frag|r:load|r:test|r:commands" name="makeVerbatimCode">
 <xsl:param name="class" value=""/>
 <xsl:param name="uid" select="generate-id(.)"/>

<!-- <xsl:param name="uid">r-<xsl:value-of select="local-name(.)"/>-<xsl:value-of select="count(ancestor::*)"/>-<xsl:value-of select="count(preceding-sibling::*)"/></xsl:param> -->

<xsl:if test="string-length(normalize-space(text())) != 0">
 <xsl:variable name="id" select="@id"/>


<div class="codeToggle">
<xsl:if test="$add-toggle-code-controls">
<a class="toggleLink"><xsl:attribute name="onclick">toggle('<xsl:value-of select="$uid"/>')</xsl:attribute>
  <!-- Why would we put this id in. Then the toggling will affect this +, not the code div
       <xsl:attribute name="id"><xsl:value-of select="$uid"/></xsl:attribute> -->
   <xsl:attribute name="title">toggle the display of code (id = <xsl:value-of select="$uid"/>)</xsl:attribute>+</a>
</xsl:if>
<div class="unhidden"><xsl:attribute name="id"><xsl:value-of select="$uid"/></xsl:attribute>
<div>
 <xsl:if test="@id">
   <a class="r-code-chunk-name"><xsl:attribute name="name"><xsl:value-of select="@id"/></xsl:attribute><!--<xsl:value-of select="@id"/>--></a>
 </xsl:if>
 
         <!-- Put links to the other containers of any r:code block with a @ref to this code block etc.  -->
   <xsl:if test="$add-link-to-refs and @id and count(//r:code[@ref=$id])"> 
<!-- Put a box around this but have it just big enough to contain the links. -->
   <div class="r-code-chunk-refs-box">
	<xsl:for-each select="//r:code[@ref=$id]">
	  <a class="r-code-chunk-ref"><a><xsl:attribute name="href">#<xsl:value-of select="../@id"/></xsl:attribute>
	  <xsl:value-of select="../@id"/></a>
	  <xsl:if test="position() != last()">, </xsl:if></a>
	</xsl:for-each>
   </div>
  </xsl:if>

  <pre class="{$class}">
   <xsl:attribute name="title"> 
    <xsl:if test="@title"><xsl:value-of select="@title"/></xsl:if>
    <xsl:if test="not(@title)">R <xsl:value-of select="local-name(.)"/><xsl:text/>
             <xsl:if test="@id"> ID: <xsl:value-of select="@id"/></xsl:if></xsl:if>
    </xsl:attribute>
<xsl:choose>
 <xsl:when test="($class = 'r' or $class = 'omg') and not(@showAsIs) and function-available('r:call')">
   <xsl:call-template name="formatRCode"/>
<!--   <xsl:value-of select="r:call('formatCode', .)"/> -->
   <xsl:apply-templates select="./r:output"/> 
 </xsl:when>
 <xsl:otherwise>
    <xsl:apply-templates/> 
 </xsl:otherwise>
</xsl:choose>
  </pre>
 </div>
</div>
</div>
<div class="clearFloat"/>
</xsl:if>
</xsl:template>

<xsl:template match="r:code[@id]|r:init[@id]|r:function[@id]" mode="toc">
<li>
 <xsl:element name="a">
   <xsl:attribute name="href">#<xsl:value-of select="@id"/></xsl:attribute>
   <xsl:value-of select="@id"/>
 </xsl:element>
</li>
</xsl:template>

<xsl:template match="r:function">
<pre class="rfunction">
 <xsl:apply-templates />
</pre>
</xsl:template>

<!-- root.messages -->


<xsl:param name="invocation"/>

<xsl:template match="invocation" name="postamble">
 <xsl:if test="not($invocation = '') and not(//invocation)">
<div>
<center><hr width="50%"></hr></center>
<PRE class="invocation">
 <xsl:value-of select="string($invocation)"/>
</PRE>
</div>
 </xsl:if>
</xsl:template>



<xsl:template match="r:pre">
<pre class="rcode"><xsl:apply-templates/></pre>
</xsl:template>

<xsl:template match="r:example">
<xsl:apply-templates/>
<center><hr width="50%"/></center>
</xsl:template>

<xsl:template match="invisible|r:invisible" />



<xsl:template match="s:var|r:var">
  <xsl:call-template name="var">
    <xsl:with-param name="class">S</xsl:with-param>
  </xsl:call-template>
</xsl:template>


<xsl:template match="docbook:note">
<div class="note">
Note:
 <xsl:apply-templates/>
</div>
</xsl:template>


<xsl:template match="datalisting|data">
 <div class="data">
 <pre class="data">
   <xsl:apply-templates />
 </pre>
 </div>
</xsl:template>

<xsl:template match="r:str|r:int|r:num|r:log">
 <code><xsl:attribute name="class">r<xsl:value-of select="local-name()"/></xsl:attribute>
  <xsl:apply-templates/>
 </code>
</xsl:template>


</xsl:stylesheet>