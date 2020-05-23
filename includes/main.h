
#define PPCAT_NX(A, B) A

typedef struct {
    int from_frame;
    int to_frame;
    char* word;
} word_frame;

typedef struct {
  word_frame** frames;
  int used;
} detected_words;

typedef struct {} unit;

typedef enum {
                 SUCCESS,               
                 FAILED_CONFIG_OBJECT,
                 FAILED_CREATE_RECOGNIZER,
                 FAILED_UNABLE_INPUTFILE
} result_code ;

typedef struct {
  result_code code;
  detected_words words;
} detect_result;

detect_result detect_words(char* filepath);
