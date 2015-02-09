<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

  <!-- Style we are overriding/extending -->
<xsl:import href="http://www.omegahat.org/XSL/html/Rhtml.xsl"/>

<xsl:import href="../common/param.xsl"/>		       <!-- parameter declarations shared with FO -->

<xsl:template match="exercise">
 <xsl:if test="$with-exercises">
  <h2 class="exercise">Exercise</h2>
   <dl class="exercise">
     <xsl:apply-templates/>
   </dl>
 </xsl:if>
</xsl:template>

<xsl:template match="q">
  <dt>
    <xsl:apply-templates select="*[not(self::a)]"/>
  </dt>
  <xsl:if test="$with-exercise-answers">
   <dd>
     <xsl:apply-templates select="./a"/>
   </dd>
  </xsl:if>
</xsl:template>



<xsl:template match="instructor">
<xsl:if test="$for-instructors">
<div class="instructor">
<img align="left" title="For instructors"><xsl:attribute name="src"><xsl:value-of select="$case-studies-icon-dir"/>/professor.<xsl:value-of select="$image-extension"/></xsl:attribute></img>


<!-- concat(concat(, '/instructors.'),$image-extension)"  -->
  <xsl:apply-templates/>
</div>
</xsl:if>
</xsl:template>

<xsl:template match="ruleOfThumb">
<div class="ruleOfThumb">
<img align="left" title="Best Practice/Rule of Thumb"><xsl:attribute name="src"><xsl:value-of select="$case-studies-icon-dir"/>/ruleOfThumb.<xsl:value-of select="$image-extension"/></xsl:attribute></img>
<para>
 <xsl:apply-templates/>
</para>
<div style="float: left;"></div>
</div>
</xsl:template>




<xsl:template name="topicList">
<xsl:param name="title"/>
<div><xsl:attribute name="class"><xsl:value-of select="local-name()"/></xsl:attribute>
<h3><xsl:attribute name="class"><xsl:value-of select="local-name()"/></xsl:attribute>
<xsl:value-of select="$title"/>
</h3> 
<xsl:for-each select="*"><xsl:apply-templates/><xsl:if test="position() != last()">, </xsl:if></xsl:for-each>
</div>
</xsl:template>

</xsl:stylesheet>

