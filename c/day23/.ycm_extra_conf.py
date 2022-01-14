def Settings(**kwargs):
    return {
      'flags': ['-x', 'c', '-Wall', '-Wextra', '-Wshadow', '-I../lib', '-DROOM_HEIGHT=2']
    }
