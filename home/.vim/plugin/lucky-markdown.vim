function! CompleteMarkdownLink(findstart, base)
  if a:findstart
    " Locate start of the matchj
    let line = getline('.')
    let start = col('.') - 1
    while start > 1 && line[start - 1] != '['
      let start -= 1
    endwhile
    return start - 1
  else
    " Do a google search
    ruby << EOF
    require 'uri'
    require 'open-uri'
    require 'json'

    link_inline = /\[([^\]]+)\]\(/
    link_ref = /\[([^\]]+)\]:/
    match = VIM.evaluate('a:base')
    api = 'http://ajax.googleapis.com/ajax/services/search/web?v=1.0&q=%s'
    VIM.command("return " +
      begin
        query = match[link_inline, 1] || match[link_ref, 1] || raise
        JSON.parse(URI.parse(api % URI.escape(query)).open.read)['responseData']['results'].map do |result|
          url = result['url']
          word = match =~ link_inline ? match + url + ')' : match + ' ' + url
          { 'word' => word, 'abbr' => url }
        end
      rescue
        []
      end.to_json
    )
EOF
  endif
endfunction

autocmd Filetype markdown setlocal omnifunc=CompleteMarkdownLink
