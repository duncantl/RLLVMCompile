<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:ltx="http://www.latex.org"
		xmlns:mml="http://www.w3.org/1998/Math/MathML"
		xmlns:r="http://www.r-project.org"
                version="1.0">

  <!-- whether to include yahoo UI code. -->
 <xsl:param name="add.yahoo.ui.code" select="0"/>
          <!-- http://www.omegahat.org/Rdocbook/YahooTabUtils.js -->
 <xsl:param name="yahoo.tab.utils.js">http://www.omegahat.org/Rdocbook/YahooTabUtils.js</xsl:param>


<xsl:template match="altApproaches">
             <!-- XXX want the index of this altApproaches relative to the entire document to get a unique name -->
  <xsl:param name="tabId">tab<xsl:value-of select="generate-id()"/></xsl:param>  <!-- <xsl:value-of select="position()"/> -->
  <xsl:comment>altApproaches <xsl:value-of select="position()"/></xsl:comment>
 
  <script type="text/javascript"> 
       <!-- We also want to emit a list of all the tab ids into a JavaScript vector
            so that we can loop over them and set them. We collect them in the JavaScript code itself.  -->
 <xsl:comment>
   var myTabs = new YAHOO.widget.TabView("<xsl:value-of select="$tabId"/>"); 
   altTabWidgets["<xsl:value-of select="$tabId"/>"] = myTabs;
 </xsl:comment>
  </script>  

  <div class="yui-navset"><xsl:attribute name="id"><xsl:value-of select="$tabId"/></xsl:attribute>
      <ul class="yui-nav">
       <xsl:for-each select="altApproach"><li><xsl:if test="position() = 1"><xsl:attribute name="class">selected</xsl:attribute></xsl:if>
         <a><xsl:attribute name="href">#tab_<xsl:value-of select="@thread"/></xsl:attribute>
            <!--<xsl:attribute name="href">#<xsl:value-of select="generate-id()"/></xsl:attribute>-->
            <!--<xsl:attribute name="thread"><xsl:value-of select="@thread"/></xsl:attribute>-->
         <xsl:value-of select="@thread"/></a></li></xsl:for-each>
      </ul>

   <div class="yui-content"> 
     <xsl:apply-templates select="altApproach"/>
  </div>
</div>
</xsl:template>

<xsl:template match="altApproach">
  <div>
    <xsl:attribute name="id"><xsl:value-of select="generate-id()"/></xsl:attribute>
    <xsl:apply-templates/>
  </div>
</xsl:template>

<xsl:template name="user.footer.content">
  <!-- Display the mathjax logo if we used mathjax.  -->
<xsl:if test="//ltx:eqn or //mml:eqn">
<p align="right"> <a href="http://www.mathjax.org/">
  <img title="Powered by MathJax"
       src="http://www.mathjax.org/badge.gif"
       border="0" alt="Powered by MathJax" />
 </a>
</p>
</xsl:if>
</xsl:template>

<xsl:template name="user.header.content">

<xsl:if test="$add-toggle-code-controls">
 <xsl:call-template name="generateToggleAllCodeJS"/>
</xsl:if>

<xsl:if test="//altApproaches">
  <!-- Put a Choose approach  and a comma-separated list of the thread/approach labels. 
       Every altApproach must have a label. -->
<div class="ApproachSelector">
<div class="ApproachSelectorTitle">Choose approach: </div>
<div class="ApproachSelectorLabels">
 <xsl:for-each select="//altApproach[not(@thread = following::altApproach/@thread)]/@thread">
   <a type="button" class="setAltApproach" href="#">
       <xsl:attribute name="onClick">setAltApproach("<xsl:value-of select="."/>");return(false);</xsl:attribute>
       <xsl:value-of select="." />
   </a>
   <xsl:if test="position() != last()">, </xsl:if>
 </xsl:for-each>
</div>
</div>
<div style="clear: both"/>
</xsl:if>
</xsl:template>

<!--/Users/duncan/Projects/JavaScript/yui/build -->
<xsl:param name="yahooCode">http://yui.yahooapis.com/2.8.0r4/build</xsl:param>

<!-- material that goes into the HEAD of the HTML document to load up yahoo UI tab view. -->
<xsl:template name="user.head.content">
<xsl:message>add.yahoo.ui.code = <xsl:value-of select="$add.yahoo.ui.code"/></xsl:message>
<xsl:if test="$add.yahoo.ui.code or count(//altApproaches) > 0">    <!--XXX determine what is a test or whether we need -->
  <xsl:text>&#010;</xsl:text>
  <link rel="stylesheet" type="text/css"><xsl:attribute name="href"><xsl:value-of select="$yahooCode"/>/tabview/assets/skins/sam/tabview.css</xsl:attribute></link>
  <xsl:text>&#010;</xsl:text>
       <!-- JavaScript Dependencies for Tabview: -->
  <script type="text/javascript"><xsl:attribute name="src"><xsl:value-of select="$yahooCode"/>/yahoo-dom-event/yahoo-dom-event.js</xsl:attribute></script>
  <xsl:text>&#010;</xsl:text>
  <script type="text/javascript"><xsl:attribute name="src"><xsl:value-of select="$yahooCode"/>/element/element-min.js</xsl:attribute></script>
  <xsl:text>&#010;</xsl:text>

     <!-- Source file for TabView -->
  <script type="text/javascript"><xsl:attribute name="src"><xsl:value-of select="$yahooCode"/>/tabview/tabview-min.js</xsl:attribute></script>

  <xsl:text>&#010;</xsl:text>
  <script type="text/javascript"><xsl:attribute name="src"><xsl:value-of select="$yahoo.tab.utils.js"/></xsl:attribute></script>
  <xsl:text>&#010;</xsl:text>
</xsl:if>

                                   <!-- The code to toggle an HTML's visible or not.  -->
<xsl:if test='$add-toggle-code-controls and not(toggle.hidden.js = "") and count(//r:plot|//r:code|//r:init|//r:frag|//r:load|//r:test|//r:commands)>0'>
  <script type="text/javascript"><xsl:attribute name="src"><xsl:value-of select="$toggle.hidden.js"/></xsl:attribute></script>
  <xsl:text>&#010;</xsl:text>
</xsl:if>

<xsl:if test="count(//ltx:eqn) > 0">
<!--<link rel="stylesheet" type="text/css" media="all" href="http://www.mathjax.org/wp-content/themes/mathjax/style.css" />-->
<script type="text/javascript" src="http://cdn.mathjax.org/mathjax/1.1-latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML-full"></script>
</xsl:if>

</xsl:template>


<xsl:template name="body.attributes">
<xsl:attribute name="class">yui-skin-sam</xsl:attribute>
</xsl:template>







<!-- Very similar to altApproaches, so consolidate -->

<xsl:template match="altImplementations">
             <!-- XXX want the index of this altApproaches relative to the entire document to get a unique name -->
  <xsl:param name="tabId">tab<xsl:value-of select="generate-id()"/></xsl:param>  <!-- <xsl:value-of select="position()"/> -->
  <xsl:comment>altImplementations <xsl:value-of select="position()"/></xsl:comment>
 
  <script type="text/javascript"> 
       <!-- We also want to emit a list of all the tab ids into a JavaScript vector
            so that we can loop over them and set them. We collect them in the JavaScript code itself.  -->
 <xsl:comment>
   var myTabs = new YAHOO.widget.TabView("<xsl:value-of select="$tabId"/>"); 
   altTabWidgets["<xsl:value-of select="$tabId"/>"] = myTabs;
 </xsl:comment>
  </script>  

  <div class="yui-navset"><xsl:attribute name="id"><xsl:value-of select="$tabId"/></xsl:attribute>
      <ul class="yui-nav">
       <xsl:for-each select="altImplementation"><li><xsl:if test="position() = 1"><xsl:attribute name="class">selected</xsl:attribute></xsl:if>
         <a><xsl:attribute name="href">#tab_<xsl:value-of select="@thread"/></xsl:attribute>
            <!--<xsl:attribute name="thread"><xsl:value-of select="@thread"/></xsl:attribute>-->
         <xsl:value-of select="@thread"/></a></li></xsl:for-each>
      </ul>


   <div class="yui-content"> 
     <xsl:apply-templates select="altImplementation"/>
  </div>
</div>
</xsl:template>



<xsl:template match="altImplementation">
  <div>
    <xsl:apply-templates/>
  </div>
</xsl:template>




</xsl:stylesheet>
