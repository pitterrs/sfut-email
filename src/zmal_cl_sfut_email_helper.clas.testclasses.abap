*"* use this source file for your ABAP unit test classes
CLASS ltc_sfut_email_helper DEFINITION
    FOR TESTING DURATION SHORT
    RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA: email_helper TYPE REF TO zmal_cl_sfut_email_helper.

    METHODS setup.

    METHODS test_name_definition FOR TESTING.

ENDCLASS.

CLASS ltc_sfut_email_helper IMPLEMENTATION.

  METHOD setup.

    me->email_helper = zmal_cl_sfut_email_helper=>factory(
        im_langu       = 'E'
        im_purch_group = `TEST` " Test Purchasing Group code
    ).

  ENDMETHOD.

  METHOD test_name_definition.

    DATA(lv_object_name) = me->email_helper->get_object_name( ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = lv_object_name
        exp = `ZMAL_SFUT_EMAIL_TEST`
        msg = `Name was wrongly defined!`
    ).

  ENDMETHOD.

ENDCLASS.
