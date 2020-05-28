
#define PPCAT_NX(A, B) A


// size = 16
typedef struct { 
  int from_frame; // 4
  int to_frame; // 4
  char* word; // 8
} word_frame;

typedef struct { // size = 16 ?!
  word_frame* frames; // 8
  int used; // 4
} detected_words;

typedef enum {
                 SUCCESS,               
                 FAILED_CONFIG_OBJECT,
                 FAILED_CREATE_RECOGNIZER,
                 FAILED_UNABLE_INPUTFILE
} result_code ;

typedef struct { // 24
  result_code code; // 4
  detected_words words; // ??
} detect_result;

detect_result* detect_words(char* filepath);
