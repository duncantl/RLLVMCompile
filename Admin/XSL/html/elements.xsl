<?xml version="1.0" ?>
<xsl:stylesheet 
        xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
        version="1.0">



<xsl:template match="note">
 <p class="note"><xsl:apply-templates /></p>
</xsl:template>


<!-- A url is mapped to <a href="url-value">url-value</a> -->
<xsl:template match="url">
 <xsl:element name="a">
  <xsl:attribute name="href">
    <xsl:value-of select="." />
  </xsl:attribute>
    <xsl:value-of select="." />
 </xsl:element>
</xsl:template>

<xsl:template match="texCmd">
<font class="texCmd">\<xsl:value-of select="." /></font>
</xsl:template>

<xsl:template match="mail">
 <xsl:element name="a">
   <xsl:attribute name="href">mailto:<xsl:value-of select="@to"/></xsl:attribute>
   <xsl:apply-templates />
 </xsl:element>
</xsl:template>


<xsl:template match="date">
 <xsl:if test="count(./day) > 0">
  <xsl:apply-templates select="./day" />/
 </xsl:if>
 <xsl:copy-of select="./month" />/
 <xsl:copy-of select="./year" />
</xsl:template>


<xsl:template match="ol">
 <ol>
  <xsl:apply-templates/>
 </ol>
</xsl:template>
<xsl:template match="ul">
 <ul>
  <xsl:apply-templates/>
 </ul>
</xsl:template>

<xsl:template match="li">
 <li>
  <xsl:apply-templates/>
 </li>
</xsl:template>

<xsl:template match="dl">
 <dl>
  <xsl:apply-templates/>
 </dl>
</xsl:template>

<xsl:template match="dd">
 <dd>
  <xsl:apply-templates/>
 </dd>
</xsl:template>

<xsl:template match="dt">
 <dt>
  <xsl:apply-templates/>
 </dt>
</xsl:template>


<xsl:template match="p|a">
 <xsl:copy-of select="."/>
</xsl:template>

<xsl:template match="em|emphasis">
 <i><xsl:apply-templates /></i>
</xsl:template>

<xsl:template match="ol">
 <ol><xsl:copy-of select="@*"/>
  <xsl:apply-templates/>
 </ol>
</xsl:template>

</xsl:stylesheet>
