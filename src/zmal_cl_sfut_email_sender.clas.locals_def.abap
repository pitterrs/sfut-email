*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

INTERFACE lif_sfut_email.

  TYPES ltr_werks TYPE RANGE OF werks_d.

  TYPES: BEGIN OF ty_email_to_be_sent,
           ekorg  TYPE ekorg,
           lifnr  TYPE lifnr,
           plants TYPE ltr_werks,
         END OF ty_email_to_be_sent.

  TYPES: BEGIN OF ty_controller,
           werks     TYPE zcsvt024d-werks,
           dispo     TYPE zcsvt024d-dispo,
           smtp_addr TYPE zcsvt024d-smtp_addr,
         END OF ty_controller.

  TYPES: BEGIN OF ty_buyer,
           ekgrp     TYPE zcsvt024-ekgrp,
           werks     TYPE zcsvt024-werks,
           smtp_addr TYPE zcsvt024-smtp_addr,
         END OF ty_buyer.

  TYPES tt_email_to_be_sent TYPE SORTED TABLE OF ty_email_to_be_sent
    WITH NON-UNIQUE KEY primary_key COMPONENTS ekorg lifnr.

  TYPES tt_header TYPE STANDARD TABLE OF zmal_sfut_email
    WITH NON-UNIQUE KEY primary_key COMPONENTS ekorg.

  TYPES tt_receiver TYPE SORTED TABLE OF zmal_sfut_recv
    WITH NON-UNIQUE KEY primary_key COMPONENTS ekorg lifnr werks.

  TYPES tt_sender TYPE SORTED TABLE OF zmal_sfut_send
    WITH NON-UNIQUE KEY primary_key COMPONENTS ekorg.

  TYPES tt_email_address TYPE TABLE OF zmal_email_address
    WITH NON-UNIQUE DEFAULT KEY.

  TYPES tt_body TYPE STANDARD TABLE OF tline
    WITH NON-UNIQUE DEFAULT KEY.

  TYPES tt_plant TYPE STANDARD TABLE OF werks_d
    WITH NON-UNIQUE DEFAULT KEY.

  TYPES tt_controller TYPE SORTED TABLE OF ty_controller
    WITH UNIQUE KEY primary_key COMPONENTS werks dispo.

  TYPES tt_buyer TYPE SORTED TABLE OF ty_buyer
    WITH UNIQUE KEY primary_key COMPONENTS ekgrp werks.

ENDINTERFACE.

**********************************************************************

CLASS lcl_sfut_email_database DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING VALUE(im_email_data)
                  TYPE zmal_sfut_supplier_data_t OPTIONAL
                VALUE(im_ignore_db_sel) TYPE abap_bool OPTIONAL.

    METHODS get_headers
      RETURNING VALUE(re_headers) TYPE lif_sfut_email=>tt_header.

    METHODS set_headers
      IMPORTING VALUE(im_headers) TYPE lif_sfut_email=>tt_header.

    METHODS get_receivers
      RETURNING VALUE(re_receivers) TYPE lif_sfut_email=>tt_receiver.

    METHODS set_receivers
      IMPORTING VALUE(im_receivers) TYPE lif_sfut_email=>tt_receiver.

    METHODS get_senders
      RETURNING VALUE(re_senders) TYPE lif_sfut_email=>tt_sender.

    METHODS set_senders
      IMPORTING VALUE(im_senders) TYPE lif_sfut_email=>tt_sender.

    METHODS get_controllers
      RETURNING VALUE(re_controllers)
                  TYPE lif_sfut_email=>tt_controller.

    METHODS set_controllers
      IMPORTING VALUE(im_controllers)
                  TYPE lif_sfut_email=>tt_controller.

    METHODS get_buyers
      RETURNING VALUE(re_buyers)
                  TYPE lif_sfut_email=>tt_buyer.

    METHODS set_buyers
      IMPORTING VALUE(im_buyers)
                  TYPE lif_sfut_email=>tt_buyer.

  PRIVATE SECTION.

    DATA headers TYPE lif_sfut_email=>tt_header. " ZMAL_SFUT_EMAIL
    DATA receivers TYPE lif_sfut_email=>tt_receiver. " ZMAL_SFUT_RECV
    DATA senders TYPE lif_sfut_email=>tt_sender. " ZMAL_SFUT_SEND

    " Data for the sender definition
    DATA controllers TYPE lif_sfut_email=>tt_controller.
    DATA buyers TYPE lif_sfut_email=>tt_buyer.

    METHODS retrieve_headers
      RETURNING VALUE(re_headers) TYPE lif_sfut_email=>tt_header.

    METHODS retrieve_receivers
      RETURNING VALUE(re_receivers) TYPE lif_sfut_email=>tt_receiver.

    METHODS retrieve_senders
      RETURNING VALUE(re_senders) TYPE lif_sfut_email=>tt_sender.

    METHODS retrieve_controllers
      IMPORTING VALUE(im_email_data)  TYPE zmal_sfut_supplier_data_t
      RETURNING VALUE(re_controllers)
                  TYPE lif_sfut_email=>tt_controller.

    METHODS retrieve_buyers
      IMPORTING VALUE(im_email_data) TYPE zmal_sfut_supplier_data_t
      RETURNING VALUE(re_buyers)     TYPE lif_sfut_email=>tt_buyer.

ENDCLASS.

**********************************************************************

CLASS lcl_sfut_email_details DEFINITION.

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING VALUE(im_database) TYPE REF TO lcl_sfut_email_database
                VALUE(im_ekorg)    TYPE ekorg
                VALUE(im_lifnr)    TYPE lifnr
                VALUE(im_plants)   TYPE lif_sfut_email=>ltr_werks.

    METHODS get_subject
      RETURNING VALUE(re_subject) TYPE zmal_email_subject.

    METHODS get_body
*      RETURNING VALUE(re_body) TYPE lif_sfut_email=>tt_body.
      RETURNING VALUE(re_body) TYPE soli_tab.

    METHODS get_receivers
      RETURNING VALUE(re_receivers)
                  TYPE lif_sfut_email=>tt_email_address.

    METHODS get_senders
      EXPORTING VALUE(ex_sender) TYPE zmal_email_address
                VALUE(ex_copy)   TYPE lif_sfut_email=>tt_email_address.

    METHODS split_attachment
      IMPORTING VALUE(im_raw_data) TYPE zmal_sfut_supplier_data_t
      RETURNING VALUE(re_filtered) TYPE zmal_sfut_supplier_data_t.

    METHODS set_database
      IMPORTING VALUE(im_database) TYPE REF TO lcl_sfut_email_database.

    METHODS get_attachment
      RETURNING VALUE(re_attachment) TYPE zmal_sfut_supplier_data_t.

    METHODS set_attachment
      IMPORTING VALUE(im_attachment) TYPE zmal_sfut_supplier_data_t.

    METHODS set_plants " Used only for testing purpose
      IMPORTING VALUE(im_plants) TYPE lif_sfut_email=>ltr_werks.

  PRIVATE SECTION.

    " Injected through the ZMAL_CL_SFUT_EMAIL_SENDER instance
    DATA database TYPE REF TO lcl_sfut_email_database.

    DATA purch_org TYPE ekorg.
    DATA supplier TYPE lifnr.
    DATA plants TYPE lif_sfut_email=>ltr_werks.

    DATA attachment TYPE zmal_sfut_supplier_data_t.

    METHODS get_disregarded_plants
      RETURNING VALUE(re_plants) TYPE lif_sfut_email=>tt_plant.

    METHODS has_sender_exception
      RETURNING VALUE(re_has_exception) TYPE abap_bool.

    METHODS has_material_and_mrp
      RETURNING VALUE(re_has_material_and_mrp) TYPE abap_bool.

    METHODS get_exception_addresses
      RETURNING VALUE(re_email_addresses)
                  TYPE lif_sfut_email=>tt_email_address.

    METHODS get_mrp_addresses
      RETURNING VALUE(re_email_addresses)
                  TYPE lif_sfut_email=>tt_email_address.

    METHODS get_buyer_addresses
      RETURNING VALUE(re_email_addresses)
                  TYPE lif_sfut_email=>tt_email_address.

ENDCLASS.

**********************************************************************

CLASS lcl_sfut_email DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING VALUE(im_details) TYPE REF TO lcl_sfut_email_details.

    CLASS-METHODS factory
      IMPORTING VALUE(im_details)  TYPE REF TO lcl_sfut_email_details
      RETURNING VALUE(re_instance) TYPE REF TO lcl_sfut_email.

    METHODS get_details
      RETURNING VALUE(re_details) TYPE REF TO lcl_sfut_email_details.

    METHODS send.

  PRIVATE SECTION.

    DATA details TYPE REF TO lcl_sfut_email_details.

    CONSTANTS document_type TYPE so_obj_tp VALUE 'RAW'.

    CONSTANTS attachment_type TYPE so_obj_tp VALUE 'XLS'.

    constants attachment_name TYPE so_obj_des VALUE 'follow_up.xlsx'.

ENDCLASS.
