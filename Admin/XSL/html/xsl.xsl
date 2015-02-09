<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
		xmlns:xi="http://www.w3.org/2003/XInclude"
                exclude-result-prefixes="xi xsl"
                version='1.0'>

<xsl:template match="xsl:code">
<pre class="xslCode">
<xsl:apply-templates/>
</pre>
</xsl:template>

</xsl:stylesheet>