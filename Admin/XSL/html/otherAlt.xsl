<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:ltx="http://www.latex.org"
		xmlns:mml="http://www.w3.org/1998/Math/MathML"
                version="1.0">

<xsl:template match="altApproaches">
<xsl:param name="tabId">tab<xsl:value-of select="generate-id()"/></xsl:param>  <!-- <xsl:value-of select="position()"/> -->
<script type="text/javascript">
    var myTabs = new YAHOO.widget.TabView("<xsl:value-of select="$tabId"/>"); 
    <xsl:apply-templates select="altApproach"/>
     
    myTabs.appendTo(document.body); 
</script>
</xsl:template>

<xsl:template match="altApproach">
 myTabs.addTab(new YAHOO.widget.Tab({
  label: "<xsl:value-of select="@thread"/>",
  content: '<xsl:apply-templates/>',
  active: <xsl:if test="position() = 1">true</xsl:if><xsl:if test="position() > 1">false</xsl:if>
})
</xsl:template>


<xsl:template match="altApproaches">
  <!-- XXX want the index of this altApproaches relative to the entire document to get a unique name -->
<xsl:param name="tabId">tab<xsl:value-of select="generate-id()"/></xsl:param>  <!-- <xsl:value-of select="position()"/> -->

<!-- We also want to emit a list of all the tab ids into a JavaScript vector
     so that we can loop over them and set them. -->

<xsl:comment>altApproaches <xsl:value-of select="position()"/></xsl:comment>

<!--  <xsl:apply-templates select="*[not(altApproaches)]"/>  -->   <!--before the first one.-->

<script type="text/javascript"> 
var myTabs = new YAHOO.widget.TabView("<xsl:value-of select="$tabId"/>"); 
</script>  
 <div class="yui-navset"><xsl:attribute name="id"><xsl:value-of select="$tabId"/></xsl:attribute>
      <ul class="yui-nav">
       <xsl:for-each select="altApproach"><li><xsl:if test="position() = 1"><xsl:attribute name="class">selected</xsl:attribute></xsl:if>
         <a><xsl:attribute name="href">#tab<xsl:value-of select="position()"/></xsl:attribute>
         <xsl:value-of select="@thread"/></a></li></xsl:for-each>
      </ul>


   <div class="yui-content"> 
     <xsl:apply-templates select="altApproach"/>
    
  </div>
 </div>
</xsl:template>

<xsl:template match="altApproach">
  <div>
    <xsl:apply-templates/>
  </div>
</xsl:template>

</xsl:stylesheet>