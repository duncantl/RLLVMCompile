<?xml version="1.0"?>

<!-- Copyright the Omegahat Project for Statistical Computing, 2000 -->
<!-- Author: Duncan Temple Lang -->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
                xmlns:bib="http://www.bibliography.org"
                xmlns:c="http://www.C.org"
                xmlns:sh="http://www.shell.org"
                xmlns:rs="http://www.omegahat.org/RS"
                xmlns:s="http://cm.bell-labs.com/stat/S4"
		xmlns:make="http://www.make.org"
                xmlns:omegahat="http://www.omegahat.org"
		exclude-result-prefixes="s"
                omit-result-prefixes="yes"
                version="1.0">

<!--
<xsl:include href="http://docbook.sourceforge.net/release/xsl/current/html/docbook.xsl" />
-->
<xsl:include href="http://docbook.sourceforge.net/release/xsl/current/html/docbook.xsl" />

<xsl:output method="html"
            encoding="ISO-8859-1"
            indent="yes"/>

<!-- Deprecated in 1.67.2 at least   xsl:param name="shade.verbatim" select="1"/ -->


 <xsl:include href="table.xsl" />
 <xsl:include href="elements.xsl" />
 <xsl:include href="java.xsl" />
 <xsl:include href="xml.xsl" /> 
 <xsl:include href="env.xsl" /> 

<!--
 <xsl:include href="http://www.omegahat.org/XSL/c.xsl" />
 <xsl:include href="http://www.omegahat.org/XSL/defs.xsl" />
 <xsl:include href="http://www.omegahat.org/XSL/SLanguage.xsl" />
 <xsl:include href="http://www.omegahat.org/XSL/Rstyle.xsl" />
 <xsl:include href="http://www.omegahat.org/XSL/omg.xsl" />
 <xsl:include href="http://www.omegahat.org/XSL/html.xsl" />
 <xsl:include href="http://www.omegahat.org/XSL/gtk.xsl" />

 <xsl:include href="http://www.omegahat.org/XSL/curl.xsl" /> 
 <xsl:include href="http://www.omegahat.org/XSL/shell.xsl" /> 
 <xsl:include href="http://www.omegahat.org/XSL/perl.xsl" /> 
 <xsl:include href="http://www.omegahat.org/XSL/system.xsl" /> 
 <xsl:include href="http://www.omegahat.org/XSL/COM.xsl" /> 
 <xsl:include href="http://www.omegahat.org/XSL/elements.xsl" /> 
 <xsl:include href="http://www.omegahat.org/XSL/make.xsl" /> 
-->
</xsl:stylesheet>
