<?xml version="1.0" encoding="UTF-8"?>

<!--https://stackoverflow.com/questions/64802207/lost-in-namespace-how-to-access-default-namespace-item-in-xslt/64803135#64803135-->
<!-- Assume that R uses libxslt 1.1.32, which is XLST 1.0 with ESL extensions  -->

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:d="http://www.w3.org/2002/xforms" xmlns:h="http://www.w3.org/1999/xhtml" xmlns:jr="http://openrosa.org/javarosa" xmlns:func="http://exslt.org/functions"
                xmlns:dyn="http://exslt.org/dynamic" extension-element-prefixes="func dyn" exclude-result-prefixes="d h">

<!--  Parameters that can be set externally-->
	<xsl:param name="lang" select="'default'"/>
	<xsl:param name="sep" select="'&#x9;'"/>

	<!-- https://stackoverflow.com/questions/64930820/setting-output-encoding-via-parameter 
	No way to set encoding via parameters.
	<xsl:param name="encoding" select="'ISO-8859-1'"/> -->
  <xsl:output method="text" encoding="ISO-8859-1" indent="no" omit-xml-declaration="yes" />
  <xsl:strip-space elements="*"/> 
   
<!--	jt:itext is used in ODK API file, we replace it by a surrogate called via evaluate-->
	<func:function name="jr:itext">
		<xsl:param name="ref"/>
		<xsl:variable name="translation" select="/h:html/h:head/d:model/d:itext/d:translation[@lang=$lang]/d:text[@id=$ref]"/>
		<func:result select="$translation"/>
	</func:function>


	<xsl:template match="h:html">
	  <xsl:value-of select="'path&#x9;name&#x9;type&#x9;field&#x9;constraint&#x9;calculate&#x9;label&#x9;hint&#xA;'" />
		<xsl:for-each select="h:head/d:model/d:bind">
			<xsl:variable name="path" select="substring-after(@nodeset, '/data')"/>
			<xsl:value-of select="$path"/>
			<xsl:value-of select="$sep"/>
			<xsl:value-of select="translate(substring-after($path,'/'), '/', '_')"/>
			<xsl:value-of select="$sep"/>
			<xsl:value-of select="@type"/>
			<xsl:value-of select="$sep"/>
			<xsl:value-of select="@constraint"/>
			<xsl:value-of select="$sep"/>
			<xsl:value-of select="@calculate"/>
			<xsl:value-of select="$sep"/>
			<xsl:variable name="fieldname" select="@nodeset"/>
			<xsl:apply-templates select="/h:html/h:body/d:*[@ref=$fieldname]"/>
			<xsl:text>&#xA;</xsl:text>
		</xsl:for-each>
	</xsl:template>

	<xsl:template match="/h:html/h:body/d:*">
		<xsl:value-of select="local-name()"/>
		<xsl:value-of select="$sep"/>
		<!-- Label and translation -->
		<xsl:variable name="ref_label" select="d:label/@ref"/>
		<xsl:choose>
			<xsl:when test="normalize-space($ref_label)">
				<xsl:value-of select="dyn:evaluate($ref_label)"/>
			</xsl:when>
			<xsl:otherwise>
				<xsl:value-of select="d:label"/>
			</xsl:otherwise>
		</xsl:choose>
		<xsl:value-of select="$sep"/>

		<!-- Hint and translation -->
		<xsl:variable name="ref_hint" select="d:hint/@ref"/>
		<xsl:choose>
			<xsl:when test="normalize-space($ref_hint)">
				<xsl:value-of select="dyn:evaluate($ref_hint)"/>
			</xsl:when>
			<xsl:otherwise>
				<xsl:value-of select="d:hint"/>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
</xsl:stylesheet><!-- Stylus Studio meta-information - (c) 2004-2009. Progress Software Corporation. All rights reserved.

<metaInformation>
	<scenarios>
		<scenario default="yes" name="odkapi" userelativepaths="yes" externalpreview="no" url="I8n_label_lang.xml" htmlbaseurl="" outputurl="" processortype="saxon8" useresolver="no" profilemode="0" profiledepth="" profilelength="" urlprofilexml=""
		          commandline="" additionalpath="" additionalclasspath="" postprocessortype="none" postprocesscommandline="" postprocessadditionalpath="" postprocessgeneratedext="" validateoutput="no" validator="internal" customvalidator="">
			<advancedProp name="sInitialMode" value=""/>
			<advancedProp name="bXsltOneIsOkay" value="true"/>
			<advancedProp name="bSchemaAware" value="false"/>
			<advancedProp name="bXml11" value="false"/>
			<advancedProp name="iValidation" value="0"/>
			<advancedProp name="bExtensions" value="true"/>
			<advancedProp name="iWhitespace" value="0"/>
			<advancedProp name="sInitialTemplate" value=""/>
			<advancedProp name="bTinyTree" value="true"/>
			<advancedProp name="bWarnings" value="true"/>
			<advancedProp name="bUseDTD" value="false"/>
			<advancedProp name="iErrorHandling" value="0"/>
		</scenario>
	</scenarios>
	<MapperMetaTag>
		<MapperInfo srcSchemaPathIsRelative="yes" srcSchemaInterpretAsXML="no" destSchemaPath="" destSchemaRoot="" destSchemaPathIsRelative="yes" destSchemaInterpretAsXML="no"/>
		<MapperBlockPosition></MapperBlockPosition>
		<TemplateContext></TemplateContext>
		<MapperFilter side="source"></MapperFilter>
	</MapperMetaTag>
</metaInformation>
-->