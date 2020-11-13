class RepeatedSubstring
  def find_repeated_substring(s)
    # catch the edge cases
    return 'NONE' if s == ''
    # check if the string consists of only one character => "aaaaaa" => "a"
    return s.split('').uniq[0] if s.split('').uniq.length == 1

    searched = []
    longest_prefix = 0
    long_prefix = ''
    (0..s.length - 1).each do |i|
      next if searched.include? s[i]

      searched.push(s[i])
      next_occurrences = next_index(s, i + 1, s[i])
      next_occurrences.each do |next_occurrence|
        next if next_occurrence == -1

        prefix = ge_prefix(s[i..next_occurrence - 1], s[next_occurrence..s.length])
        if prefix.length > longest_prefix
          longest_prefix = prefix.length
          long_prefix = prefix
        end
      end
    end
    # if prefix == "       " it is a invalid sequence
    return 'NONE' if long_prefix.strip.empty?

    long_prefix
  end

  def get_prefix(s1, s2)
    prefix = ''
    min_length = [s1.length, s2.length].min
    return '' if s1.nil? || s2.nil?

    (0..min_length - 1).each do |i|
      return prefix if s1[i] != s2[i]

      prefix += s1[i]
    end
    prefix
  end

  def next_index(seq, index, value)
    indexes = []
    (index..seq.length).each do |i|
      indexes.push(i) if seq[i] == value
    end
    indexes
  end

  def find_repeated_substring_file(file_path)
    File.open(file_path).read.each_line.map { |line| find_repeated_substring(line) }
  end
end
