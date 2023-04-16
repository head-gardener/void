// Global store indexes
enum store_indexes {
  STORE_TOOLBAR_DROPDOWNS = 0,
  STORE_SPREADSHEET_INPUT_FIELD,
};

// Marks
enum draw_queue_marks {
  MARKS_TOOLBAR = 1,
  MARKS_SPREADSHEET,
  MARKS_TOOLBAR_DROPDOWN_TABLE,
  MARKS_SPREADSHEET_INPUT_FIELD,
  MARKS_MENU_CURSOR,
};
enum click_sink_marks {
  MARKS_TOOLBAR_CLICK_SINK = 0,
  MARKS_SPREADSHEET_CLICK_SINK,
};
enum text_sink_marks {
  MARKS_SPREADSHEET_TEXT_SINK = 0,
};
enum key_sink_marks {
  MARKS_TOOLBAR_KEY_SINK = 0,
};

// Bit masks
#define MASK_TOOLBAR_DROPDOWN_TABLE 1
