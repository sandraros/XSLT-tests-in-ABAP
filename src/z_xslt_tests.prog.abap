*&---------------------------------------------------------------------*
*& Report z_xslt_tests
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_xslt_tests.

CLASS lcx_r3tr_xtra DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.

CLASS lcl_r3tr_xtra DEFINITION.
  PUBLIC SECTION.

    "! Create a transformation object (R3TR XTRA). Call must be followed by COMMIT WORK.
    "! @parameter i_xsltname | Name of transformation object
    "! @parameter i_xslt_source | Can be in Simple Transformation or XSLT.
    CLASS-METHODS create_update_r3tr_xtra_object
      IMPORTING
        i_xsltname           TYPE clike
        VALUE(i_xslt_source) TYPE string
      RAISING
        lcx_r3tr_xtra.

ENDCLASS.

CLASS lcl_r3tr_xtra IMPLEMENTATION.

  METHOD create_update_r3tr_xtra_object.
    DATA: lo_xslt   TYPE REF TO cl_o2_api_xsltdesc,
          ls_attr   TYPE o2xsltattr,
          lt_source TYPE o2pageline_table.

    DATA(normalized_xslt_source) = replace( val = i_xslt_source sub = |\r\n| with = |\n| occ = 0 ).

    ls_attr-xsltdesc = i_xsltname.
    IF cl_o2_api_xsltdesc=>exists( p_xslt_desc = ls_attr-xsltdesc ) = '1'.
      cl_o2_api_xsltdesc=>load(
        EXPORTING
          p_xslt_desc = ls_attr-xsltdesc
        IMPORTING
          p_obj       = lo_xslt
        EXCEPTIONS
          OTHERS      = 1 ).
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE lcx_r3tr_xtra.
      ENDIF.
      lo_xslt->set_changeable(
        EXPORTING
          p_changeable                   = abap_true
        EXCEPTIONS
          action_cancelled               = 1
          error_occured                  = 2
          object_already_changeable      = 3
          object_already_unlocked        = 4
          object_invalid                 = 5
          object_just_created            = 6
          object_locked_by_other_user    = 7
          object_modified                = 8
          permission_failure             = 9
          OTHERS                         = 10 ).
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE lcx_r3tr_xtra.
      ENDIF.
      cl_o2_api_xsltdesc=>prepare_source_table(
        IMPORTING
          e_source_table = lt_source
        CHANGING
          i_string       = normalized_xslt_source ).
      lo_xslt->set_source(
        EXPORTING
          p_source = lt_source
        EXCEPTIONS
          OTHERS   = 1 ).
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE lcx_r3tr_xtra.
      ENDIF.
    ELSE.
*      cl_o2_api_xsltdesc=>create_new_from_string(
*        EXPORTING
*          p_source = normalized_xslt_source
*          p_attr   = ls_attr
*        IMPORTING
*          p_obj    = lo_xslt
*        EXCEPTIONS
*          OTHERS   = 1 ).
*      IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_r3tr_xtra.
*      ENDIF.
    ENDIF.
    lo_xslt->save(
      EXPORTING
        i_source_state         = cl_o2_api_xsltdesc=>c_report_state_active
        i_suppress_corr_insert = 'X'
      EXCEPTIONS
        OTHERS                 = 1 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_r3tr_xtra.
    ENDIF.
    lo_xslt->activate(
      EXCEPTIONS
        OTHERS = 1 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_r3tr_xtra.
    ENDIF.
    lo_xslt->generate(
      IMPORTING
        e_error_list = DATA(e_error_list)
      EXCEPTIONS
        OTHERS = 1 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_r3tr_xtra.
    ENDIF.
    lo_xslt->set_changeable(
      EXPORTING
        p_changeable                   = abap_false
      EXCEPTIONS
        action_cancelled               = 1
        error_occured                  = 2
        object_already_changeable      = 3
        object_already_unlocked        = 4
        object_invalid                 = 5
        object_just_created            = 6
        object_locked_by_other_user    = 7
        object_modified                = 8
        permission_failure             = 9
        OTHERS                         = 10 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_r3tr_xtra.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS ltc_key DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS test FOR TESTING RAISING cx_static_check.
    METHODS test2 FOR TESTING RAISING cx_static_check.

    METHODS transfo
      IMPORTING
        xml_in         TYPE csequence
        xslt           TYPE csequence
      RETURNING
        VALUE(xml_out) TYPE string
      RAISING
        lcx_r3tr_xtra.

ENDCLASS.

CLASS ltc_key IMPLEMENTATION.

  METHOD test.
    cl_abap_unit_assert=>assert_equals(
        act = transfo( xml_in = |<root><t>A</t><s>B</s><t>C</t></root>|
                       xslt   = |<xsl:key name="kT" match="/root/t" use="string(.)"/>      \n| &
                                |<xsl:template match="/">                                  \n| &
                                |  <root2>                                                 \n| &
                                |    <t2><xsl:value-of select="key('kT',/root/t[1])"/></t2>\n| &
                                |    <t2><xsl:value-of select="key('kT',/root/t[2])"/></t2>\n| &
                                |  </root2>                                                \n| &
                                |</xsl:template>                                           \n| )
        exp = |<root2><t2>A</t2><t2>C</t2></root2>| ).
  ENDMETHOD.

  METHOD test2.
    cl_abap_unit_assert=>assert_equals(
        act = transfo( xml_in = |<root><t>A</t><s>B</s><t>C</t></root>|
                       xslt   = |<xsl:key name="kT" match="/root/t" use="string(.)"/>      \n| &
                                |<xsl:template match="/">                                  \n| &
                                |  <root2>                                                 \n| &
                                |    <t2><xsl:value-of select="key('kT',/root/t[1])"/></t2>\n| &
                                |    <t2><xsl:value-of select="key('kT',/root/t[2])"/></t2>\n| &
                                |  </root2>                                                \n| &
                                |</xsl:template>                                           \n| )
        exp = |<root2><t2>A</t2><t2>C</t2></root2>| ).
  ENDMETHOD.

  METHOD transfo.
    lcl_r3tr_xtra=>create_update_r3tr_xtra_object(
        i_xsltname    = 'Z_XSLT_TEST'
        i_xslt_source = |<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">\n| &&
                        |<xsl:output indent="no" method="xml"/>\n| &&
                        xslt &&
                        |</xsl:stylesheet>| ).
    COMMIT WORK AND WAIT.
    CALL TRANSFORMATION ('Z_XSLT_TEST') SOURCE XML xml_in RESULT XML xml_out OPTIONS xml_header = 'no'.
    " Remove UTF BOM
    IF xml_out IS NOT INITIAL.
      xml_out = xml_out+1.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
