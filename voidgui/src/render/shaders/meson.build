embed = find_program('./embed.sh', native: true)

shaders = [
    'common.vert',
    'common.frag',
    'tex.vert',
    'tex.frag',
]

foreach name : shaders
    output = name.underscorify() + '_src.h'
    var = name.underscorify() + '_src'
    sources += custom_target(
        output,
        command: [embed, var],
        input: name,
        output: output,
        feed: true,
        capture: true,
    )
endforeach
