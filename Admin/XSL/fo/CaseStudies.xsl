<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:fo="http://www.w3.org/1999/XSL/Format"
                version="1.0">

  <!-- Style we are overriding/extending -->
<xsl:import href="http://www.omegahat.org/XSL/fo/Rfo.xsl"/>

<xsl:import href="../common/param.xsl"/>		       <!-- parameter declarations shared with FO -->


<xsl:template match="exercise">

<!--
<xsl:message>Foo</xsl:message>
<xsl:for-each select="*[local-name()]">
<xsl:message><xsl:value-of select="name()"/></xsl:message>
</xsl:for-each>
<xsl:message>/Foo</xsl:message>
-->

 <xsl:if test="$with-exercises">
  <fo:inline>Exercise</fo:inline>
   <fo:list-block provisional-label-separation="2pt">
     <xsl:apply-templates select="*[local-name()]"/>
   </fo:list-block>
 </xsl:if>
</xsl:template>

<xsl:template match="q">

  <fo:list-item>
  <fo:list-item-label><fo:block>Q <xsl:value-of select="position()"/></fo:block></fo:list-item-label>
  <fo:list-item-body start-indent="body-start()">
   <fo:block><xsl:apply-templates/></fo:block>
  </fo:list-item-body>
  <!--  <xsl:apply-templates select="*[not(self::a)]"/> -->
  </fo:list-item>
  <xsl:if test="$with-exercise-answers">
   <dd>
     <xsl:apply-templates select="./a"/>
   </dd>
  </xsl:if>
</xsl:template>

<xsl:template match="a" />


<xsl:template match="data">
 <fo:block hyphenate="false" font-family="monospace" text-align="start" 
         wrap-option="no-wrap" linefeed-treatment="preserve" white-space-treatment="preserve" 
          white-space-collapse="false"> 
  <xsl:apply-templates/>
 </fo:block>
</xsl:template>


<!-- Not sure we can get the text to wrap around the image and be level with it.
     We can use a <fo:float float="start"> but FOP doesn't seem to handle this. Some discussion on
     mailing lists. -->
<xsl:template match="instructor">
<xsl:if test="$for-instructors">
<fo:block border-style="solid">
<fo:block>
</fo:block>
<fo:block>
<!-- <fo:float float="start"> </fo:float> -->
  <fo:external-graphic content-height="50%" content-width="50%">
     <xsl:attribute name="src">url(<xsl:value-of select="$case-studies-icon-dir"/>/professor.<xsl:value-of select="$image-extension"/>)</xsl:attribute></fo:external-graphic>

  <xsl:apply-templates/>
</fo:block>
</fo:block>
</xsl:if>
</xsl:template>

<!-- Same issue with image as instructors -->
<xsl:template match="ruleOfThumb">

<fo:block border-style='solid'>
<fo:external-graphic><xsl:attribute name="src"><xsl:value-of select="$case-studies-icon-dir"/>/ruleOfThumb.<xsl:value-of select="$image-extension"/></xsl:attribute></fo:external-graphic>
 <xsl:apply-templates/>
</fo:block>
</xsl:template>


<xsl:template name="topicList">
<xsl:param name="title"/>
<fo:block>
<fo:inline font-weight="bold"><xsl:value-of select="$title"/></fo:inline>: 
<xsl:for-each select="*"><xsl:apply-templates/><xsl:if test="position() != last()">, </xsl:if></xsl:for-each>
</fo:block>
</xsl:template>



</xsl:stylesheet>

