<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:css="http://www.w3.org/Style/CSS/"
		exclude-result-prefixes="css"
                version="1.0">

<xsl:template match="css:selector">
  <b class="css-selector"><xsl:apply-templates/></b>
</xsl:template>

</xsl:stylesheet>