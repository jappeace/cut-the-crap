#include <pocketsphinx.h>
#define PPCAT_NX(A, B) A

int main(int argc, char *argv[])
{
    const cmd_ln_t* config = cmd_ln_init(NULL, ps_args(), TRUE,
		         "-hmm", MODELDIR "/en-us/en-us",
	                 "-lm", MODELDIR "/en-us/en-us.lm.bin",
	                 "-dict", MODELDIR "/en-us/cmudict-en-us.dict",
	                 NULL);

    if (config == NULL) {
	fprintf(stderr, "Failed to create config object, see log for details\n");
	return -1;
    }

    const ps_decoder_t* ps = ps_init(config);
    if (ps == NULL) {
	fprintf(stderr, "Failed to create recognizer, see log for details\n");
	return -1;
    }

    // If you want to create your own version of it: it needs to be a single-channel (monaural), little-endian, unheadered 16-bit signed PCM audio file sampled at 16000 Hz.
    FILE* fh = fopen("heyo.raw", "rb");
    if (fh == NULL) {
	fprintf(stderr, "Unable to open input file goforward.raw\n");
	return -1;
    }
    int rv = ps_start_utt(ps);
    int16 buf[512];

    while (!feof(fh)) {
	size_t nsamp;
	nsamp = fread(buf, 2, 512, fh);
	rv = ps_process_raw(ps, buf, nsamp, FALSE, FALSE);
    }

    rv = ps_end_utt(ps);
    int32 score;
    char const * hyp = ps_get_hyp(ps, &score);
    printf("Recognized: %s\n", hyp);

    fclose(fh);
    ps_free(ps);
    cmd_ln_free_r(config);
    printf("outputting results " MODELDIR);
    return 0;
}
