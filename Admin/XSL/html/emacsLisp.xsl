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
		xmlns:el="http://www.gnu.org/software/emacs/manual/elisp.html"
		exclude-result-prefixes="r s rs c bib el omegahat"
                omit-result-prefixes="yes"
                version="1.0">


<xsl:template match="el:key">
 <code class="emacsKey"><xsl:apply-templates/></code>
</xsl:template>

<xsl:template match="el:var">
 <code class="emacsVariable"><xsl:apply-templates/></code>
</xsl:template>

<xsl:template match="el:func">
 <code class="emacsFunction"><xsl:apply-templates/></code>
</xsl:template>

</xsl:stylesheet>