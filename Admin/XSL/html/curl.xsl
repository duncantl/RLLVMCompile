<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
                xmlns:curl="http://curl.haxx.se"
                version="1.0">

<xsl:template match="curl:opt">
<font color="red" class="curlOpt"><xsl:apply-templates /></font>
</xsl:template>

</xsl:stylesheet>