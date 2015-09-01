#ifdef linux
    #include "io/sg.c"
#elif defined(WIN32)
    #include "io/aspi.c"
#else
    #error Unknown OS type
#endif
