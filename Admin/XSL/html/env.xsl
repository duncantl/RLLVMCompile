<?xml version="1.0"?>

<!-- Copyright the Omegahat Project for Statistical Computing, 2000 -->
<!-- Author: Duncan Temple Lang -->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
                xmlns:env="http://www.shell.org"
                version="1.0">


<xsl:template match="env:var">
 <b><xsl:apply-templates /></b>
</xsl:template>

<xsl:template match="env:code">
<pre class="shell">
 <xsl:apply-templates />
</pre>
</xsl:template>

</xsl:stylesheet>