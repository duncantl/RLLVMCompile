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
		exclude-result-prefixes="r s rs c bib"
                omit-result-prefixes="yes"
                version="1.0">

<xsl:output method="html"
            encoding="ISO-8859-1"
            indent="yes"/>

<!-- 
<xsl:include href="http://www.omegahat.org/XSL/myarticle.xsl"/>
 -->

<xsl:template match="//r:example">
 <HTML>
  <HEAD>
   <title><xsl:value-of select="./title"/></title>
  </HEAD>
  <BODY>
   <xsl:value-of select="./package/@name"/>
     <br/>
   <center><xsl:value-of select="./title"/></center>
    <xsl:apply-templates/>
  </BODY>
 </HTML>
</xsl:template>

<xsl:template match="r:example/title"/>

</xsl:stylesheet>
