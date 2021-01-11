REPORT zmalr_sfut_email_receiver.

CONSTANTS co_file_ext TYPE string VALUE '*.xlsx'.

SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE text-001.
PARAMETERS p_path TYPE zexcel_export_dir.
SELECTION-SCREEN END OF BLOCK a.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  PERFORM get_file_path CHANGING p_path.

START-OF-SELECTION.

END-OF-SELECTION.

FORM get_file_path CHANGING pv_path TYPE zexcel_export_dir.

  DATA lt_file_table TYPE filetable.
  DATA lv_action TYPE i.
  DATA lv_subrc TYPE i.

  cl_gui_frontend_services=>file_open_dialog(
    EXPORTING
      default_extension       = co_file_ext
      default_filename        = co_file_ext
      file_filter             = |{ text-002 } ({ co_file_ext })|
    CHANGING
      file_table              = lt_file_table
      rc                      = lv_subrc
      user_action             = lv_action
  ).

  pv_path = COND #(
    WHEN lv_action = cl_gui_frontend_services=>action_ok
    AND lv_subrc = 1
    THEN lt_file_table[ 1 ]
  ).

ENDFORM.
