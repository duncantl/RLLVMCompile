<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:r="http://www.r-project.org"
		xmlns:s="http://cm.bell-labs.com/stat/S4"
		xmlns:s3="http://www.r-project.org/S3"
		xmlns:c="http://www.C.org"
		xmlns:cpp="http://www.cplusplus.org"
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
                xmlns:xi="http://www.w3.org/2001/XInclude"
		xmlns:html="http://www.w3.org/TR/html401"
		exclude-result-prefixes="s r c python bioc gtk com sh java omegahat perl vb statdocs html cpp xi s3 "
                version="1.0">
<!-- Customization layer -->

<!-- Use 'chunk.xsl' in line below to chunk files. -->
<!-- <xsl:import href="http://docbook.sourceforge.net/release/xsl/current/html/docbook.xsl"/> -->

<!-- We probably want a local version here for use in Firefox. -->
<!-- xhtml or html -->
<xsl:import href="http://docbook.sourceforge.net/release/xsl/current/xhtml/docbook.xsl" />

<xsl:import href="make.xsl"/>
<xsl:import href="SLanguage.xsl"/>
<xsl:import href="../common/COM.xsl"/>

<xsl:import href="c.xsl"/>

<xsl:import href="xml.xsl"/>
<xsl:import href="gtk.xsl"/>
<!-- ???  <xsl:import href="latex.xsl"   is this the one in ../latex/latex.xsl and if so why do we want it? /> -->
<xsl:import href="shell.xsl"/>
<xsl:import href="omg.xsl"/>
<xsl:import href="js.xsl"/>
<xsl:import href="Rsource.xsl"/>
<xsl:import href="defs.xsl"/>
<xsl:import href="html.xsl"/>
<xsl:import href="curl.xsl"/>
<xsl:import href="regex.xsl"/>

<xsl:import href="svg.xsl"/>

<xsl:import href="emacsLisp.xsl"/>

<xsl:import href="alt.xsl"/>

<xsl:import href="css.xsl"/>
<xsl:import href="xsl.xsl"/>

<xsl:param name="codebox" select="0"/>

<xsl:param name="add.defn.tooltips" select="1"/>

<xsl:param name="html.stylesheet" select="'S.css'"/> <!-- doc:type='string'  -->
<!--<xsl:param name="html.stylesheet">Bob.css</xsl:param>-->

<xsl:param name="generate.section.toc.level" select="1"/>  <!--  doc:type='int'/> -->
<xsl:param name="generate.toc" select='1'/>
<xsl:param name="runCode" select="0"/>
<xsl:param name="rhelp.dir" select="'Help/'" />
<xsl:param name="add.links.to.rfuncs" select="0" />

<xsl:param name="bioc.release.number" select="2.6"/>

<xsl:template match="dots"><b>...</b></xsl:template>


<xsl:template match="xi:include">
<xsl:element name="a">
 <xsl:attribute name="href"><xsl:value-of select="@href"/></xsl:attribute>
 <xsl:value-of select="@href"/>
</xsl:element>
</xsl:template>


<xsl:template match="r" name="R.ref"><b class="R">R</b></xsl:template>

<xsl:template name="code">
<xsl:param name="class">code</xsl:param>
<xsl:if test="@xml:id"><xsl:element name="a"><xsl:attribute name="name"><xsl:value-of select="@xml:id"/></xsl:attribute></xsl:element></xsl:if>
<pre class="{string($class)}">
   <xsl:apply-templates/>
</pre>

<!--
 <table border="{$codebox}">
 <tr align="left"><th>
 <xsl:if test="@xml:id"><xsl:element name="a"><xsl:attribute name="name"><xsl:value-of select="@xml:id"/></xsl:attribute></xsl:element></xsl:if>
 <pre class="{string($class)}" align="left">
   <xsl:apply-templates/>
 </pre>
 </th></tr>
</table>
-->
<br/>
</xsl:template>

<!-- In SLanguage.xsl (the same)
<xsl:template name="var">
  <b class="${class}"><xsl:apply-templates/></b>
</xsl:template>
-->




<xsl:template name="addTooltip">
   <xsl:param name="name" select="string(.)"/>
   <xsl:param name="node" select="name()"/>
   <xsl:if test="$add.defn.tooltips">
    <xsl:attribute name="title">
      <xsl:choose><xsl:when test="@def"><xsl:value-of select="@def"/></xsl:when>
                         <!-- we arrange to match based on the name of this node, i.e. r:func, or r:pkg,
                               i.e. find the node with the def but only of the same type as this node. -->
                  <xsl:otherwise><xsl:value-of select="//*[name() = $node and @def and string(.) = $name]/@def"/></xsl:otherwise>
      </xsl:choose>
    </xsl:attribute>
   </xsl:if>
</xsl:template>

<xsl:template match="s:functionDef|s:function|r:function" name="funcDef">
  <xsl:call-template name="code">
    <xsl:with-param name="class">rfunction</xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template match="r:classDef" >
  <xsl:call-template name="code">
    <xsl:with-param name="class">rclassDef</xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template match="r:methodDef" >
  <xsl:call-template name="code">
    <xsl:with-param name="class">rmethod</xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template match="cpp:code">
  <xsl:call-template name="code">
    <xsl:with-param name="class">cpp</xsl:with-param>
  </xsl:call-template>
</xsl:template>


<xsl:template match="r:arg|s:arg|r:param">
 <i class="rarg"><xsl:apply-templates/></i>
</xsl:template>

<xsl:template match="r:slot|s:slot">
  <i class="rslot"><xsl:apply-templates/></i>
</xsl:template>

<xsl:template match="r:el|s:el">
  <i class="relement"><xsl:apply-templates/></i>
</xsl:template>

<xsl:template match="r:namespace">
 <b class="namespace"><xsl:apply-templates/></b>
</xsl:template>

<xsl:template match="c:routine|c:method">
 <i>
  <xsl:element name="a">
   <xsl:attribute name="href"><xsl:value-of select="name(.)"/>("<xsl:value-of select="."/>")</xsl:attribute>
   <xsl:apply-templates/>
  </xsl:element>
  </i>
</xsl:template>



<xsl:template match="omegahat:package">
 <i>
  <xsl:element name="a">
   <xsl:attribute name="href">http://www.omegahat.org/<xsl:value-of select="."/></xsl:attribute>
   <xsl:apply-templates/>
  </xsl:element>
  </i>
</xsl:template>


<xsl:template match="statdocs:package">
 <i>
  <xsl:element name="a">
   <xsl:attribute name="href">http://www.statdocs.org/<xsl:value-of select="."/></xsl:attribute>
   <xsl:apply-templates/>
  </xsl:element>
  </i>
</xsl:template>



<xsl:template match="bioc:package">
 <i>
  <xsl:element name="a">
   <xsl:attribute name="href">http://www.bioconductor.org/<xsl:value-of select="."/></xsl:attribute>
   <xsl:apply-templates/>
  </xsl:element>
  </i>
</xsl:template>


<!-- Perhaps change the name here to html unless we know we can deal 
     with XML files all the way. -->
<xsl:template match="r:include">
 <xsl:element name="a">
  <xsl:attribute name="href"><xsl:value-of select="doc"/>#<xsl:value-of select="@ref"/></xsl:attribute>
  <xsl:value-of select="@ref"/>
 </xsl:element>
</xsl:template>

<xsl:template match="s:class|r:class|c:class|java:class|com:class|com:interface">
 <i>
  <xsl:element name="a">
   <xsl:attribute name="href"><xsl:value-of select="$rhelp.dir"/><xsl:value-of select="."/>-class.html</xsl:attribute>
   <xsl:apply-templates/>
  </xsl:element>
  </i>
</xsl:template>


<xsl:template match="s:class[@file]|r:class[@file]">
 <i>
  <xsl:element name="a">
   <xsl:attribute name="href"><xsl:value-of select="$rhelp.dir"/><xsl:value-of select="@file"/>-class.html</xsl:attribute>
   <xsl:apply-templates/>
  </xsl:element>
  </i>
</xsl:template>


<xsl:template match="s:class[@name]|r:class[@name]">
 <i>
  <xsl:element name="a">
   <xsl:attribute name="href"><xsl:value-of select="$rhelp.dir"/><xsl:value-of select="@name"/>-class.html</xsl:attribute>
   <xsl:value-of select="@name"/>
  </xsl:element>
  </i>
</xsl:template>



<xsl:template match="@*"> <xsl:value-of select="."/></xsl:template>

<xsl:template match="hr|sub|sup|i|object|param|table[not(tgroup) and not(r:code)]|tr|td|th|h1|h2|h3|h4|h5|h6|code|form|font|html|head|body|OBJECT|PARAM|br|option|select|optgroup|input">
<xsl:message>Passing a node (<xsl:value-of select="name()"/> -> <xsl:value-of select="string(.)"/>) through to HTML</xsl:message>
 <xsl:element name="{name(.)}">
  <xsl:for-each select="@*">
   <xsl:copy />
  </xsl:for-each>
   <xsl:apply-templates />
 </xsl:element>
</xsl:template>

<xsl:template match="key">
 <b><xsl:apply-templates /></b>
</xsl:template>


<xsl:template match="s:prompt">
 <b>&gt;</b>
</xsl:template>






<xsl:template match="r:error">
 <xsl:if test="not($runCode)">
   <pre class="rerror"><xsl:apply-templates/></pre>
 </xsl:if>
</xsl:template>


<xsl:template match="s:warning|r:warning">
 <pre class="rwarning"><xsl:apply-templates/></pre>
</xsl:template>


<xsl:template match="directory|dir">
  <xsl:call-template name="inline.monoseq"/>/
</xsl:template>

<xsl:template match="todo">
 <b>Todo:</b><i class="todo"><xsl:apply-templates /></i> 
</xsl:template>

<xsl:template match="r:data|r:object|r:value" />

<xsl:template match="r:s3class|s3:class">
 <code class="s3class"><xsl:apply-templates/></code>
</xsl:template>


<!-- Some "constants"  -->
<xsl:template match="docbook">
 <a href="http://www.docbook.org">DocBook</a>
</xsl:template>

<xsl:template match="unix">
 UNIX&#x00AE;
</xsl:template>

<xsl:template match="omegahat">
<a href="http://www.omegahat.org">Omegahat</a>
</xsl:template>

<xsl:template match="codeEmphasis">
 <font class="codeEmphasis" col="red"><xsl:apply-templates /></font>
</xsl:template>


<!-- A math variable -->
<xsl:template match="var">
<code class="mathVar"><xsl:call-template name="addTooltip"/><xsl:apply-templates/></code>
</xsl:template>

</xsl:stylesheet>
