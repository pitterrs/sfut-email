*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_sfut_email IMPLEMENTATION.

  METHOD constructor.
    me->details = im_details.
  ENDMETHOD.

  METHOD factory.
    re_instance = NEW #( im_details ).
  ENDMETHOD.

  METHOD get_details.
    re_details = me->details.
  ENDMETHOD.

  METHOD send.

    DATA lt_rawdata   TYPE solix_tab.
    DATA lv_bytecount TYPE sood-objlen.
    DATA lt_attachment_header TYPE soli_tab.
    DATA lr_address TYPE REF TO cl_cam_address_bcs.
    DATA lv_sender TYPE zmal_email_address.
    DATA lt_copy TYPE lif_sfut_email=>tt_email_address.

    " Create the excel file based on the attachment data
    zmal_cl_sfut_email_excel=>factory( )->create_excel(
      EXPORTING
        delinq_table_raw = me->details->get_attachment( )
      IMPORTING
        ex_rawdata       = lt_rawdata
        ex_bytecount     = lv_bytecount
    ).

    DATA(lr_bcs) = cl_bcs=>create_persistent( ).

    " Set message subject on request to enable 50+ chars on subj
    lr_bcs->set_message_subject(
      me->details->get_subject( )
    ).

    DATA(lr_document) = cl_document_bcs=>create_document(
      i_type        = lcl_sfut_email=>document_type
      i_subject     = CONV so_obj_des( me->details->get_subject( ) )
      i_text        = me->details->get_body( )
    ).

    " Create attachment header
    APPEND VALUE #(
        line = |&SO_FILENAME={ lcl_sfut_email=>attachment_name }|
    ) TO lt_attachment_header.

    " Add the attachment data to the e-mail object
    lr_document->add_attachment(
      EXPORTING
        i_attachment_type     = lcl_sfut_email=>attachment_type
        i_attachment_subject  = lcl_sfut_email=>attachment_name
        i_attachment_size     = lv_bytecount
        i_att_content_hex     = lt_rawdata
        i_attachment_header   = lt_attachment_header
    ).
    lr_bcs->set_document( lr_document ).

    " Set receivers
    LOOP AT me->details->get_receivers( ) INTO DATA(lv_receiver).
      lr_address = cl_cam_address_bcs=>create_internet_address(
        CONV ad_smtpadr( lv_receiver )
      ).
      lr_bcs->add_recipient( lr_address ).
      FREE lr_address.
    ENDLOOP.

    " Set sender and CC's
    me->details->get_senders(
      IMPORTING
        ex_sender = lv_sender
        ex_copy   = lt_copy
    ).

    lr_address = cl_cam_address_bcs=>create_internet_address(
      CONV ad_smtpadr( lv_sender )
    ).
    lr_bcs->set_sender( lr_address ).
    FREE lr_address.

    " Set e-mail CC's
    LOOP AT lt_copy INTO DATA(lv_copy).
      lr_address = cl_cam_address_bcs=>create_internet_address(
        CONV ad_smtpadr( lv_copy )
      ).
      lr_bcs->add_recipient(
        EXPORTING
          i_recipient     = lr_address
          i_copy          = abap_true
      ).
      FREE lr_address.
    ENDLOOP.

    " Send the e-mail procedure
    re_sent = lr_bcs->send( i_with_error_screen = abap_true ).

  ENDMETHOD.

ENDCLASS.

**********************************************************************

CLASS lcl_sfut_email_database IMPLEMENTATION.

  METHOD constructor.

    IF im_ignore_db_sel IS INITIAL.

      " Get the subject and body data through the ZMAL_SFUT_EMAIL table
      me->headers = me->retrieve_headers( ).

      " Get the e-mail to data through the ZMAL_SFUT_RECV table
      me->receivers = me->retrieve_receivers( ).

      " Get the e-mail from data through the ZMAL_SFUT_SEND table
      me->senders = me->retrieve_senders( ).

      CHECK im_email_data IS NOT INITIAL.

      " Select the ZCSVT024D table
      me->controllers = me->retrieve_controllers( im_email_data ).

      " Select the ZCSVT024 table
      me->buyers = me->retrieve_buyers( im_email_data ).

    ENDIF.

  ENDMETHOD.

  METHOD get_headers.
    re_headers = me->headers.
  ENDMETHOD.

  METHOD set_headers.
    me->headers = im_headers.
  ENDMETHOD.

  METHOD get_receivers.
    re_receivers = me->receivers.
  ENDMETHOD.

  METHOD set_receivers.
    me->receivers = im_receivers.
  ENDMETHOD.

  METHOD get_senders.
    re_senders = me->senders.
  ENDMETHOD.

  METHOD set_senders.
    me->senders = im_senders.
  ENDMETHOD.

  METHOD retrieve_headers.

    SELECT mandt
           ekorg
           subject
      FROM zmal_sfut_email
      INTO TABLE re_headers.

  ENDMETHOD.

  METHOD retrieve_receivers.

    SELECT mandt
           ekorg
           lifnr
           werks
           email
      FROM zmal_sfut_recv
      INTO TABLE re_receivers.

  ENDMETHOD.

  METHOD retrieve_senders.

    SELECT mandt
           ekorg
           email
      FROM zmal_sfut_send
      INTO TABLE re_senders.

  ENDMETHOD.

  METHOD retrieve_controllers.

    DATA lt_filtered_email_data TYPE TABLE OF zmal_sfut_supplier_data.

    " Prepare data for the ZCSVT024D table access
    lt_filtered_email_data = im_email_data.
    SORT lt_filtered_email_data BY werks dispo.
    DELETE ADJACENT DUPLICATES FROM lt_filtered_email_data
    COMPARING werks dispo.

    IF lt_filtered_email_data IS NOT INITIAL.

      " Retrieve MRP Controller e-mails from the database
      SELECT werks
             dispo
             smtp_addr
      FROM zcsvt024d
      INTO TABLE re_controllers
      FOR ALL ENTRIES IN lt_filtered_email_data
      WHERE werks = lt_filtered_email_data-werks
      AND   dispo = lt_filtered_email_data-dispo.

      CHECK sy-subrc IS INITIAL.

      " Ignore registers with empty e-mail
      DELETE re_controllers
      WHERE smtp_addr IS INITIAL.

    ENDIF.

  ENDMETHOD.

  METHOD retrieve_buyers.

    DATA lt_filtered_email_data TYPE TABLE OF zmal_sfut_supplier_data.

    " Prepare data for the ZCSVT024 table access
    lt_filtered_email_data = im_email_data.
    SORT lt_filtered_email_data BY ekgrp werks.
    DELETE ADJACENT DUPLICATES FROM lt_filtered_email_data
    COMPARING ekgrp werks.

    IF lt_filtered_email_data IS NOT INITIAL.

      " Retrieve Buyer e-mails from the database
      SELECT ekgrp
             werks
             smtp_addr
      FROM zcsvt024
      INTO TABLE re_buyers
      FOR ALL ENTRIES IN lt_filtered_email_data
      WHERE ekgrp = lt_filtered_email_data-ekgrp
      AND   werks = lt_filtered_email_data-werks.

      CHECK sy-subrc IS INITIAL.

      " Ignore registers with empty e-mail
      DELETE re_buyers
      WHERE smtp_addr IS INITIAL.

    ENDIF.

  ENDMETHOD.

  METHOD get_controllers.
    re_controllers = me->controllers.
  ENDMETHOD.

  METHOD set_controllers.
    me->controllers = im_controllers.
  ENDMETHOD.

  METHOD get_buyers.
    re_buyers = me->buyers.
  ENDMETHOD.

  METHOD set_buyers.
    me->buyers = im_buyers.
  ENDMETHOD.

ENDCLASS.

**********************************************************************

CLASS lcl_sfut_email_details IMPLEMENTATION.

  METHOD constructor.
    " Inject settings data to avoid multiple database accesses
    me->database = im_database.
    me->purch_org = im_ekorg.
    me->supplier = im_lifnr.
    me->plants = im_plants.
  ENDMETHOD.

  METHOD get_purch_org.
    re_purch_org = me->purch_org.
  ENDMETHOD.

  METHOD get_supplier.
    re_supplier = me->supplier.
  ENDMETHOD.

  METHOD get_plants.
    re_plants = me->plants.
  ENDMETHOD.

  METHOD get_plants_as_text.
    LOOP AT me->plants INTO DATA(lv_plant).
      re_text = COND #(
          WHEN re_text IS INITIAL
          THEN lv_plant
          ELSE |{ re_text }, { CONV text4( lv_plant ) }|
      ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_subject.

    " Concatenate the registered text with the purchasing
    " organization and supplier data for the e-mails subject
    DATA(lt_headers) = me->database->get_headers( ).

    DATA(lw_header) = VALUE #(
        lt_headers[ ekorg = me->purch_org ] DEFAULT space
    ).

    re_subject = |{ lw_header-subject } - | && |Purch Org: | &&
        |{ me->purch_org } / Supplier: { me->supplier }|.

  ENDMETHOD.

  METHOD get_body.

    DATA lt_text_stream TYPE string_table.

    " Use the helper class created for the body content configuration
    DATA(lt_text_component) = zmal_cl_sfut_email_helper=>factory(
        me->purch_org
    )->read_text( ).

    " Convert from STRING_TABLE to SOLI_TAB to add it to the e-mail
    CALL FUNCTION 'CONVERT_ITF_TO_STREAM_TEXT'
      TABLES
        itf_text      = lt_text_component
        text_stream   = re_body
      EXCEPTIONS
        error_message = 1
        OTHERS        = 2.

  ENDMETHOD.

  METHOD get_receivers.

    DATA lt_receivers TYPE lif_sfut_email=>tt_receiver.

    " Filter by the key at the receivers table (ZMAL_SFUT_RECEIVER)
    " As the plants are being grouped the first register at the plants
    " list can be considered for the contacts retrieving process
    lt_receivers = FILTER #(
          me->database->get_receivers( )
          USING KEY primary_key
          WHERE ekorg = me->purch_org
          AND lifnr = me->supplier
          AND werks = me->plants[ 1 ]-low
    ).

    " Return a list of e-mail addresses
    re_receivers = VALUE lif_sfut_email=>tt_email_address(
        FOR lw_receiver IN lt_receivers (
            lw_receiver-email
        )
    ).

  ENDMETHOD.

  METHOD get_senders.

    DATA lt_senders TYPE lif_sfut_email=>tt_sender.

    IF me->has_sender_exception( ). " Consider the ZMAL_SFUT_SEND table
      lt_senders = me->get_exception_addresses( ).
    ELSE. " Use the MRP controller or the Buyer e-mail
      IF me->has_material_and_mrp( ). " Send through the MRP controller
        lt_senders = me->get_mrp_addresses( ).
      ELSE. " Send through the Buyer e-mail
        lt_senders = me->get_buyer_addresses( ).
      ENDIF.
    ENDIF.

    ex_sender = lt_senders[ 1 ]-email. " Set main sender

    " Set e-mail CC...
*    CHECK lines( lt_senders ) > 1.
    ex_copy = VALUE lif_sfut_email=>tt_email_address(
*      " Avoid add the sender e-mail as CC
*      FOR lw_sender IN lt_senders FROM 2 ( lw_sender-email )
      " Send to the sender as CC
      FOR lw_sender IN lt_senders ( lw_sender-email )
    ).

  ENDMETHOD.

  METHOD split_attachment.

    re_filtered = im_raw_data. " Get all data to apply the filters

    " Check if there is registers with no plant (get all other plants)
    IF line_exists( me->plants[ low = '' ] ).
      re_filtered = FILTER #(
          im_raw_data USING KEY primary_key
          WHERE ekorg = me->purch_org
          AND lifnr   = me->supplier
      ).
    ELSE. " Delete registers that are out of the plants list instance
      DELETE re_filtered
      WHERE ekorg <> me->purch_org
      OR lifnr <> me->supplier
      OR werks NOT IN me->plants.
    ENDIF.

    CHECK line_exists( me->plants[ low = '' ] ).

    " Remove other plants that are registered with the same purchasing
    " organization and sup data considering the email grouping logic
    LOOP AT me->get_disregarded_plants( ) INTO DATA(lv_plant).
      DELETE re_filtered
      WHERE werks = lv_plant.
    ENDLOOP.

  ENDMETHOD.

  METHOD set_database.
    me->database = im_database.
  ENDMETHOD.

  METHOD get_attachment.
    re_attachment = me->attachment.
  ENDMETHOD.

  METHOD set_attachment.
    me->attachment = im_attachment.
  ENDMETHOD.

  METHOD set_plants.
    me->plants = im_plants.
  ENDMETHOD.

  METHOD get_disregarded_plants.

    DATA lt_receivers TYPE lif_sfut_email=>tt_receiver.

    " Get plants with the same Purchase Org and Supplier
    lt_receivers = FILTER #(
      me->database->get_receivers( )
      USING KEY primary_key
      WHERE ekorg = me->purch_org
      AND   lifnr = me->supplier
    ).

    " Unnecessary to consider registers with no plant
    DELETE lt_receivers
    WHERE werks IS INITIAL.

    " Disregard the e-mail for the plants consolidation operation
    DELETE ADJACENT DUPLICATES FROM lt_receivers
    USING KEY primary_key.

    " Remove plants from the e-mail grouping logic
    DELETE lt_receivers
    WHERE werks IN me->plants.

    " Return a list of plants
    re_plants = VALUE lif_sfut_email=>tt_plant(
        FOR lw_receiver IN lt_receivers (
            lw_receiver-werks
        )
    ).

  ENDMETHOD.

  METHOD has_sender_exception.

    DATA(lt_senders) = me->database->get_senders( ).

    re_has_exception = COND #(
        WHEN line_exists( lt_senders[ ekorg = me->purch_org ] )
        THEN abap_true ELSE abap_false
    ).

  ENDMETHOD.

  METHOD has_material_and_mrp.

    DATA(lt_orders) = me->attachment.
    DELETE lt_orders
    WHERE matnr IS INITIAL
    OR dispo IS INITIAL.

    re_has_material_and_mrp = COND #(
        WHEN lt_orders IS NOT INITIAL
        THEN abap_true ELSE abap_false
    ).

  ENDMETHOD.

  METHOD get_exception_addresses.

    re_email_addresses = FILTER #(
        me->database->get_senders( )
        USING KEY primary_key
        WHERE ekorg = me->purch_org
    ).

  ENDMETHOD.

  METHOD get_mrp_addresses.

    re_email_addresses = VALUE lif_sfut_email=>tt_email_address(
        FOR lw_controller IN me->database->get_controllers( ) (
            COND #(
                WHEN line_exists( me->attachment[
                    werks = lw_controller-werks
                    dispo = lw_controller-dispo
                ] )
                THEN CONV zmal_sfut_email( lw_controller-smtp_addr )
            )
        )
    ).

  ENDMETHOD.

  METHOD get_buyer_addresses.

    re_email_addresses = VALUE lif_sfut_email=>tt_email_address(
        FOR lw_buyer IN me->database->get_buyers( ) (
            COND #(
                WHEN line_exists( me->attachment[
                    ekgrp = lw_buyer-ekgrp
                    werks = lw_buyer-werks
                ] )
                THEN CONV zmal_sfut_email( lw_buyer-smtp_addr )
            )
        )
    ).

  ENDMETHOD.

ENDCLASS.
