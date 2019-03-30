"! <strong>Übersichtsliste Datei-Zugriffe aus Prozessen</strong>
"! <br/>
"! Diese Klasse demonstriert den externen Zugriff auf MCI-Konfigurationsdaten.
"! Nutzen Sie die Transaktion ZMCIBUCH_FILE_OVRVW zum Start des Beispiels.
"! <br/>
"! Diese Klasse ist eine nicht gewartete Beispiel-Implementierung zur Ergänzung
"! des Buchs "i.s.h.med MCI", ISBN 978-3-7469-1883-9. Nutzung auf eigene Gefahr.
"! Beachten Sie die Hinweise in Kapitel 9.5
"! <br/>
"! THIS PROGRAM IS PROVIDED UNDER THE TERMS OF THE ECLIPSE PUBLIC LICENSE. ANY
"! USE, REPRODUCTION OR DISTRIBUTION OF THE PROGRAM CONSTITUTES RECIPIENT'S
"! ACCEPTANCE OF THIS AGREEMENT.
CLASS zcl_mcibuch_file_overview DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.
    "! Aufbau und Anzeige der Liste
    METHODS start.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA overview_list TYPE STANDARD TABLE OF zmcibuch_file_overview_entry.
    DATA alv_table TYPE REF TO cl_salv_table.
    DATA num_processes_examined TYPE i.
    DATA config_load_time TYPE i.

    "! Erstellt die Liste zur Anzeige.
    METHODS build_list.

    "! Zeigt die Liste in einer ALV-Sicht an.
    METHODS display_list.

    "! Fügt einen Datei-Konnektor zur Liste hinzu.
    METHODS add_connector_entry
      IMPORTING
        i_process        TYPE rn1mci_proc
        i_component_type TYPE n1mci_component_type
        i_configuration  TYPE REF TO cl_ishmed_mci_file_cfg.

    "! Erzeugt ein Symbol mit Tooltip zur Anzeige.
    METHODS create_icon
      IMPORTING
        i_icon          TYPE icon_l2
        i_tooltip       TYPE csequence
      RETURNING
        VALUE(r_result) TYPE n1mci_info_icon.

    "! Nimmt die Spalteneinstellungen der ALV-Sicht vor.
    METHODS adjust_column_settings
      IMPORTING
        i_columns TYPE REF TO cl_salv_columns_table
      RAISING
        cx_salv_not_found.

ENDCLASS.



CLASS ZCL_MCIBUCH_FILE_OVERVIEW IMPLEMENTATION.


  METHOD add_connector_entry.

    " Kopfeintrag vorbelegen
    DATA(new_entry) = VALUE zmcibuch_file_overview_entry(
      process_group        = i_process-pgroup
      process_name         = i_process-name
      process_text         = i_process-text
      process_status       = i_process-status
      process_status_icon  = SWITCH #( i_process-status
        WHEN if_ishmed_mci_comp_constant=>co_proc_active
             THEN create_icon(
                      i_icon    = icon_activate
                      i_tooltip = 'aktiv'(007)
                  )
        WHEN if_ishmed_mci_comp_constant=>co_proc_inactive
             THEN create_icon(
                      i_icon    = icon_deactivate
                      i_tooltip = 'inaktiv'(008)
                  )
      )
      component_type       = i_component_type
    ).

    " Daten je nach Konnektor-Typ beschaffen
    CASE i_component_type.
      WHEN if_ishmed_mci_comp_constant=>co_start_connector.
        new_entry-component_type_icon = create_icon(
            i_icon    = if_ishmed_mci_icon_constant=>co_icon_start_connector
            i_tooltip = 'Start-Konnektor'(005)
        ).
        new_entry-import_directory = i_configuration->get_directory( ).
        new_entry-import_pattern   = i_configuration->get_file_pattern( ).

        " Quell-Pfad zusammensetzen - siehe ISHMED_MCI_GET_FILE_LIST
        CALL FUNCTION 'FILE_GET_NAME_AND_LOGICAL_PATH'
          EXPORTING
            logical_filename = new_entry-import_directory
          IMPORTING
            logical_path     = new_entry-import_path
          EXCEPTIONS
            OTHERS           = 1.
        IF sy-subrc = 0.
          CALL FUNCTION 'FILE_GET_NAME_USING_PATH'
            EXPORTING
              logical_path        = new_entry-import_path
              file_name           = new_entry-import_pattern
            IMPORTING
              file_name_with_path = new_entry-effective_target
            EXCEPTIONS
              OTHERS              = 0.
        ENDIF.

      WHEN if_ishmed_mci_comp_constant=>co_end_connector.
        new_entry-component_type_icon = create_icon(
            i_icon    = if_ishmed_mci_icon_constant=>co_icon_end_connector
            i_tooltip = 'End-Konnektor'(006)
        ).
        new_entry-export_filename  = i_configuration->get_filename( ).
        new_entry-export_extension = i_configuration->get_file_extension( ).

        " Ziel-Pfad zusammensetzen - siehe ISHMED_MCI_WRITE_FILE
        CALL FUNCTION 'FILE_GET_NAME'
          EXPORTING
            logical_filename = new_entry-export_filename
            parameter_1      = new_entry-export_extension
            parameter_2      = '$MSG_CONTROL_ID$'
          IMPORTING
            file_name        = new_entry-effective_target
          EXCEPTIONS
            OTHERS           = 0.

    ENDCASE.
    APPEND new_entry TO me->overview_list.

  ENDMETHOD.


  METHOD adjust_column_settings.

    DATA(col_process_name) = CAST cl_salv_column_table(
      i_columns->get_column( 'PROCESS_NAME' )
    ).
    col_process_name->set_key( abap_true ).

    DATA(col_process_status) = CAST cl_salv_column_table(
      i_columns->get_column( 'PROCESS_STATUS' )
    ).
    col_process_status->set_technical( abap_true ).

    DATA(col_process_status_icon) = CAST cl_salv_column_table(
      i_columns->get_column( 'PROCESS_STATUS_ICON' )
    ).
    col_process_status_icon->set_alignment( if_salv_c_alignment=>centered ).
    col_process_status_icon->set_short_text( CONV #( 'Status'(002) ) ).
    col_process_status_icon->set_medium_text( CONV #( 'Status'(002) ) ).
    col_process_status_icon->set_long_text( CONV #( 'Status'(002) ) ).

    DATA(col_component_type) = CAST cl_salv_column_table(
      i_columns->get_column( 'COMPONENT_TYPE' )
    ).
    col_component_type->set_technical( abap_true ).

    DATA(col_component_type_icon) = CAST cl_salv_column_table(
      i_columns->get_column( 'COMPONENT_TYPE_ICON' )
    ).
    col_component_type_icon->set_alignment( if_salv_c_alignment=>centered ).
    col_component_type->set_short_text( CONV #( 'Typ'(003) ) ).
    col_component_type->set_medium_text( CONV #( 'Konnektor-Typ'(004) ) ).
    col_component_type->set_long_text( CONV #( 'Konnektor-Typ'(004) ) ).

    DATA(col_import_path) = CAST cl_salv_column_table(
      i_columns->get_column( 'IMPORT_PATH' )
    ).
    col_import_path->set_visible( abap_false ).

    DATA(col_effective_target) = CAST cl_salv_column_table(
      i_columns->get_column( 'EFFECTIVE_TARGET' )
    ).
    col_effective_target->set_color( VALUE #(
      col = cl_gui_resources=>list_col_positive
    ) ).

  ENDMETHOD.


  METHOD build_list.

    FREE me->overview_list.
    FREE me->num_processes_examined.
    FREE me->config_load_time.

    " alle Prozesse laden und durchgehen
    GET RUN TIME FIELD DATA(start_time).
    DATA(processes) = cl_ishmed_mci_process_data=>get_processes( ).
    GET RUN TIME FIELD DATA(end_time).
    me->config_load_time = me->config_load_time + end_time - start_time.
    SORT processes BY pgroup name.
    LOOP AT processes ASSIGNING FIELD-SYMBOL(<process>).

      " Konfiguration laden
      TRY.
          GET RUN TIME FIELD start_time.
          DATA(configuration) = cl_ishmed_mci_process_config=>load( <process>-name ).

          " Start-Konnektor prüfen
          configuration->get_start_connector(
            IMPORTING
              e_class = DATA(connector_class)
          ).
          IF connector_class = 'CL_ISHMED_MCI_SINGLE_FILE_CONN'.
            DATA(connector_configuration) = CAST cl_ishmed_mci_file_cfg(
              configuration->get_configuration(
                  i_step_id = if_ishmed_mci_comp_constant=>co_stepid_start
              )
            ).
            GET RUN TIME FIELD end_time.
            me->config_load_time = me->config_load_time + end_time - start_time.
            add_connector_entry(
                i_process        = <process>
                i_component_type = if_ishmed_mci_comp_constant=>co_start_connector
                i_configuration  = connector_configuration
            ).
          ENDIF.

          " End-Konnektor prüfen
          GET RUN TIME FIELD start_time.
          configuration->get_end_connector(
            IMPORTING
              e_class = connector_class
          ).
          IF connector_class = 'CL_ISHMED_MCI_SINGLE_FILE_CONN'.
            connector_configuration = CAST cl_ishmed_mci_file_cfg(
              configuration->get_configuration(
                  i_step_id = if_ishmed_mci_comp_constant=>co_stepid_end
              )
            ).
            GET RUN TIME FIELD end_time.
            me->config_load_time = me->config_load_time + end_time - start_time.
            add_connector_entry(
                i_process        = <process>
                i_component_type = if_ishmed_mci_comp_constant=>co_end_connector
                i_configuration  = connector_configuration
            ).
          ENDIF.

          me->num_processes_examined = me->num_processes_examined + 1.

        CATCH cx_ishmed_not_found cx_ishmed_mci_config.
          " Prozess überspringen
          CONTINUE.
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.


  METHOD create_icon.
    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name       = i_icon
        info       = i_tooltip
        add_stdinf = CONV icon_int( abap_false )
      IMPORTING
        result     = r_result.
  ENDMETHOD.


  METHOD display_list.

    " Konfiguration von &1 Prozessen in &2 us gelesen
    MESSAGE s038(zmcibuch) WITH me->num_processes_examined me->config_load_time.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table   = me->alv_table
          CHANGING
            t_table        = me->overview_list
        ).

        DATA(display_settings) = me->alv_table->get_display_settings( ).
        display_settings->set_list_header(
            value = CONV #( 'Übersicht Dateizugriffe aus MCI-Prozessen'(001) )
        ).

        DATA(functions) = me->alv_table->get_functions( ).
        functions->set_default( ).

        DATA(columns) = me->alv_table->get_columns( ).
        columns->set_optimize( abap_true ).
        adjust_column_settings( columns ).

        me->alv_table->display( ).

      CATCH cx_salv_msg cx_salv_not_found INTO DATA(alv_error).
        MESSAGE alv_error TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.


  METHOD start.
    build_list( ).
    display_list( ).
  ENDMETHOD.
ENDCLASS.
