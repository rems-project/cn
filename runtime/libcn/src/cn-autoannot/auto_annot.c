#include <cn-autoannot/auto_annot.h>
#include <cn-executable/utils.h>
#include <stdlib.h>

// Single definition of the global log file pointer
FILE *auto_annot_log_file = NULL;

//void initialise_focus_context();

void initialize_auto_annot(const char *log_file) {
   auto_annot_log_file = fopen(log_file, "a");
   if (auto_annot_log_file == NULL) {
     cn_printf(CN_LOGGING_ERROR, "Failed to open log file: %s\n", log_file);
     exit(EXIT_FAILURE);
   }
   initialise_focus_context();
}

void finalize_auto_annot() {
    fclose(auto_annot_log_file);
}
