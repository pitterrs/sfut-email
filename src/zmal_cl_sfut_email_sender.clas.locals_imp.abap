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

    DATA(lr_excel) = NEW zmal_cl_sfut_email_excel( ).

    lr_excel->create_excel(
      EXPORTING
        delinq_table_raw = me->details->get_attachment( )
      IMPORTING
        ex_rawdata       = lt_rawdata
        ex_bytecount     = lv_bytecount
    ).

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
    me->plant = im_werks.
  ENDMETHOD.

  METHOD get_subject.

    " Concatenate the registered text with the purchasing
    " organization and supplier data for the e-mails subject
    DATA(lt_headers) = me->database->get_headers( ).

    DATA(lw_header) = VALUE #(
        lt_headers[ ekorg = me->purch_org ] DEFAULT space
    ).

    re_subject = COND #(
      WHEN me->plant IS INITIAL
      THEN |{ lw_header-subject } - | && |Purch Org: | &&
        |{ me->purch_org } / Supplier: { me->supplier }|
      ELSE |{ lw_header-subject } - | && |Purch Org: | &&
        |{ me->purch_org } / Supplier: { me->supplier } | &&
        |/ Plant: { me->plant }|
    ).

  ENDMETHOD.

  METHOD get_body.

    " Use the helper class created for the body content configuration
    re_body = zmal_cl_sfut_email_helper=>factory(
        me->purch_org
    )->read_text( ).

  ENDMETHOD.

  METHOD get_receivers.

    DATA lt_receivers TYPE lif_sfut_email=>tt_receiver.

    " Filter by the key at the receivers table (ZMAL_SFUT_RECEIVER)
    lt_receivers = FILTER #(
          me->database->get_receivers( )
          USING KEY primary_key
          WHERE ekorg = me->purch_org
          AND lifnr = me->supplier
          AND werks = me->plant
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
    CHECK lines( lt_senders ) > 1.
    ex_copy = VALUE lif_sfut_email=>tt_email_address(
      FOR lw_sender IN lt_senders FROM 2 ( lw_sender-email )
    ).

  ENDMETHOD.

  METHOD get_sup_data.

    " Load data logic attending the presence of the plant information
    re_filtered = COND #(
      WHEN me->plant IS INITIAL THEN
        FILTER #(
          im_raw_data USING KEY primary_key
          WHERE ekorg = me->purch_org
          AND lifnr   = me->supplier
        )
      ELSE " Consider the plant for the filter...
        FILTER #(
          im_raw_data USING KEY primary_key
          WHERE ekorg = me->purch_org
          AND lifnr   = me->supplier
          AND werks   = me->plant
        )
    ).

    CHECK me->plant IS INITIAL.

    " Remove other plants that are registered with the same purchasing
    " organization and supplier data
    LOOP AT me->get_plants( ) INTO DATA(lv_disregarded_plant).
      DELETE re_filtered
      WHERE werks = lv_disregarded_plant.
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

  METHOD set_plant.
    me->plant = im_werks.
  ENDMETHOD.

  METHOD get_plants.

    DATA lt_receivers TYPE lif_sfut_email=>tt_receiver.

    lt_receivers = FILTER #(
      me->database->get_receivers( )
      USING KEY primary_key
      WHERE ekorg = me->purch_org
      AND   lifnr = me->supplier
    ).

    DELETE lt_receivers
    WHERE werks IS INITIAL.

    DELETE ADJACENT DUPLICATES FROM lt_receivers
    USING KEY primary_key.

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
