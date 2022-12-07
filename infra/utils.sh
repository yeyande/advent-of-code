get_base_directory() {
  git rev-parse --show-toplevel
}

get_session_token() {
  cat $(get_base_directory)/infra/login_session
}

get_day_problem() {
  local year=$1
  local day=$2
  python $(get_base_directory)/infra/parse_problem.py --day "$day" --year "$year" --session-file "$(get_base_directory)/infra/login_session"
}

get_day_input() {
  local year=$1
  local day=$(echo $2 | sed "s/^0\([0-9]\)/\1/")
  set -x
  curl https://adventofcode.com/$year/day/$day/input --header "cookie: session=$(get_session_token)" > input.txt
}

init_year() {
  local year=$1
  local language=$2
  mkdir -p $(get_base_directory)/$year
  echo $language > $(get_base_directory)/$year/.language
}

init_day() {
  local day=$1
  local year=$2
  if [ -z $day ]; then
    day=$(date +"%d")
  fi
  if [ -z $year ]; then
    year=$(date +"%Y")
  fi
  local year_dir="$(get_base_directory)/$year"
  local language=$(cat $year_dir/.language)
  case $language in 
    rust) 
      cargo new --name day-$day "$year_dir/$day"
      ;;
    *)
      mkdir -p "$year_dir/$day"
      ;;
  esac
  pushd "$year_dir/$day"
  get_day_problem $year $day
  get_day_input $year $day
  popd
}
