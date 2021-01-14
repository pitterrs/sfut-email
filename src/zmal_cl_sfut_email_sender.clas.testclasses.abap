*"* use this source file for your ABAP unit test classes

*CLASS ltc_sfut_email_details DEFINITION
*    FOR TESTING DURATION SHORT
*    RISK LEVEL HARMLESS.
*
*  PRIVATE SECTION.
*
*    METHODS setup.
*
*    DATA database TYPE REF TO lcl_sfut_email_database.
*
*    DATA email_details TYPE REF TO lcl_sfut_email_details.
*
*    CONSTANTS email_subject TYPE zmal_email_subject VALUE 'Subj'.
*    CONSTANTS purch_org TYPE ekorg VALUE '9991'.
*    CONSTANTS supplier TYPE lifnr VALUE '99991'.
*    CONSTANTS plant TYPE werks_d VALUE '9992'.
*
*    CONSTANTS correct_email TYPE zmal_email_address
*        VALUE `test1@JohnDeere.com`.
*
*    METHODS add_test_data.
*
*    METHODS test_subject_with_plant FOR TESTING.
*
*    METHODS test_subject_without_plant FOR TESTING.
*
*    METHODS test_get_receivers FOR TESTING.
*
*ENDCLASS.
*
*CLASS ltc_sfut_email_details IMPLEMENTATION.
*
*  METHOD setup.
*
*    " Simulate the selection process for the e-mail data
*    me->database = NEW #( im_ignore_db_sel = abap_true ).
*
*    " Add data to the database object for test purpose
*    me->add_test_data( ).
*
*    " Create an instance of the e-mail details object for tests
*    me->email_details = NEW lcl_sfut_email_details(
*          im_database = me->database
*          im_ekorg    = ltc_sfut_email_details=>purch_org
*          im_lifnr    = ltc_sfut_email_details=>supplier
*    ).
*
*  ENDMETHOD.
*
*  METHOD add_test_data.
*
*    " Add details (Simulate ZMAL_SFUT_EMAIL)
*    me->database->set_headers( VALUE #( (
*        mandt = sy-mandt
*        ekorg = ltc_sfut_email_details=>purch_org
*        subject = ltc_sfut_email_details=>email_subject
*    ) ) ).
*
*    " Add receivers (Simulate ZMAL_SFUT_RECV)
*    me->database->set_receivers( VALUE #(
*        (
*            mandt = sy-mandt
*            ekorg = `1234`
*            lifnr = `12345`
*            werks = `1234`
*            email = `test@JohnDeere.com`
*        )
*        (
*            mandt = sy-mandt
*            ekorg = ltc_sfut_email_details=>purch_org
*            lifnr = ltc_sfut_email_details=>supplier
*            werks = ltc_sfut_email_details=>plant
*            email = ltc_sfut_email_details=>correct_email
*        )
*    ) ).
*
*  ENDMETHOD.
*
*  METHOD test_subject_with_plant.
*
*    " Test will be performed with the plant information
*    me->email_details->set_plant( ltc_sfut_email_details=>plant ).
*
*    DATA(lv_expected) = |Subj - Purch Org: 9991 / | &&
*    |Supplier: 99991 / Plant: 9992|.
*    DATA(lv_subject) = me->email_details->get_subject( ).
*
*    cl_abap_unit_assert=>assert_equals(
*      EXPORTING
*        act = lv_subject
*        exp = lv_expected
*        msg = `Subject was wrongly defined!`
*    ).
*
*  ENDMETHOD.
*
*  METHOD test_subject_without_plant.
*
*    " Test will be performed without the plant information
*    me->email_details->set_plant( space ).
*
*    DATA(lv_expected) = |Subj - Purch Org: 9991 / Supplier: 99991|.
*    DATA(lv_subject) = me->email_details->get_subject( ).
*
*    cl_abap_unit_assert=>assert_equals(
*      EXPORTING
*        act = lv_subject
*        exp = lv_expected
*        msg = `Subject was wrongly defined!`
*    ).
*
*  ENDMETHOD.
*
*  METHOD test_get_receivers.
*
*    " Test will be performed with the plant information
*    me->email_details->set_plant( ltc_sfut_email_details=>plant ).
*
*    DATA(lt_receivers) = me->email_details->get_receivers( ).
*    DATA(lw_receiver) = lt_receivers[ 1 ].
*
*    cl_abap_unit_assert=>assert_equals(
*      EXPORTING
*        act = lw_receiver
*        exp = ltc_sfut_email_details=>correct_email
*        msg = `Receiver was wrongly defined!`
*    ).
*
*  ENDMETHOD.
*
*ENDCLASS.
