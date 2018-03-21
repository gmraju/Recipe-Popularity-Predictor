library(dplyr)
library(jsonlite)
library(stringr)
library(tidyr)
library(tibble)
library(reshape2)

dat <- fromJSON(txt = "/Users/marisolromero/OneDrive/Northwestern/2016-2017/Q3/EECS 349-0/Project/Final.json", flatten = TRUE)
dat <- unique(dat)

sapply(dat, class)

ingredient_list <- dat$ingredientLines

ingredient_list <- lapply(ingredient_list, tolower)
ingredient_list <- lapply(ingredient_list, function(x) gsub("[^a-z ]","",x))
ingredient_list <- lapply(ingredient_list, function(x) x[nchar(x) < 50])
ingredient_list <- lapply(ingredient_list, function(x) x[!grepl("optional",x)])

details <- paste0("if you have a fresh orange just squeeze by hand|", 
                  "trimmed|sqeezed|half|lengthwise|sliced|",
                  "tidbits|unsweetened|sweetened|tenderloin|unsalted|salted|for garnish|smashed|eigths|",
                  "grainy|red mill|organic|festive|motts|container|tasty kitchen|",
                  "pioneer womans|fun sized|sized|size|thirds|separated|broken|strips|",
                  "used|bunches|bunched|bunch|bundles|bundle|",
                  "grated|about|topping|diced|small|mediumsized|medium|",
                  "freshly|fresh|bite|size|pieces|minced|heads|head|package|",
                  "large|tablespoons|tablespoon|cooked|pkg|instructions|leaving|them a|",
                  "them|firmly|firm|orange or red or yellow|serving|divided|",
                  "into|pinch|inchthick|inch|piece|frying|preferably|topshelf|seeded|",
                  "grated|thinly|to taste|taste|traditionalists|ounces|ounce|teaspoon|",
                  "teaspoons|shredded|delallo|cholula|rinsed|drained|chopped|",
                  "other|slow cooker|you|also|use|version|versions|see note below|",
                  "see note|quicker|alternatives|packets|brewed|lightly|beaten|ground|",
                  "plus|sticks|stick|softened|dashes|dash|perdue|bonein|each|coarse|finely|fine|",
                  "smoked|dried|zested|zest|semisweet|old el paso|stand n stuff|",
                  "granulated|creamy|chunky|drizzling|roughly|whole|foods|packs|",
                  "found|dry|allnatural|sifted|halved|diagonally|",
                  "slices|slice|peeled|hardboiled|toasted|ribbons|marcona|",
                  "extravirgin|virgin|classic|old fashioned|packed|temperature|",
                  "allpurpose|ripe|like|your|favorite|store|bought|crushed|florets|lean|",
                  "shelled|such as lawrys|gala|cubed|sriracha|leaves|any variety|",
                  "reduced fat|slivered|from chipotle pepper|from|green valley|",
                  "stone|thin|cloves|clove|crumbled|flatleaf|shaved|sodium|needed|",
                  "well|pure|more|fatfree|lite|shaken|kept hot|nonfat| for$|",
                  " at$|preferred|juiced| or$|melted|stacys|boxes|box|blanched|strained|unfiltered|melted|",
                  "oldfashioned|frontier|quartered|yulu|panskillet|necessary|original|full|to serve|prepared|",
                  "all natural|handful|stick|clear|fluid|pickled|fat free|just|under|washed|scraped|choice|",
                  "membranes removed|all purpose|flavored|pitted|land o lakes|lukewarm|brand|desired|",
                  "glutenfree|gluten free|sifting|powdered|light|gold medal|almond breeze|texas|thick|",
                  "sheet|mixed|grain|boneless|skinless|pit removed|skin removed|roll|athens|defrosted|",
                  "squares|grams|gram|boston|cubes|cube|section|only|fillets|circles|california|hass|",
                  "stems|removed|remove|homemade|chilled|leftover|lowfat|panko|meyer|slightly|",
                  "free range|any kind|approx|weight|wedges|granny smith|vine |chunked|plain|serve|",
                  "flavor|yoplait|duncan hines|golden butter|mashed|canned|captain morgan|braeburn|",
                  "juicy|sturdy|sweetart|winesap|natural|petite|refrigerated|such as|quarts|seedless|",
                  "frozen|packet|baby|sprinkled|sprinkle|campfire|spears|condensed|instant|squeezed|",
                  "stemmed|thawed|stalks|stalk|italianstyle|vermont creamery| rounds|tiny|tender insides|dusting|",
                  "sprinkling|reduced|griddle|total|devils food|hulled|cannellini|blue diamond|russet|smart|",
                  "dutch process|equal|anjou barlett|seattles best|salemville|no seeds|halves|equal|pan drippings|follows|recipe|",
                  "veggie blendins|josh|didnt|this|fried|tbps|dutchprocess|quarters|quarter|fire roasted|",
                  "msgfree|type")

details2 <- paste0(" can | cans | or | to | i | in | ive | sea | not | low | and | of | for | if | etc\\. |",
                   "etc | oz  | etc$| extra | raw |^raw | cup |^cup |^cups | cups | tsp | tbsp | lb |^ozs|^oz| at$|",
                   "^lb | bit | lbs |^lbs |^cans |^of |^can |^tsp |^tbsp |tbs|^s |^ofs|^s |^cups| cut |^cut |",
                   " pound | pounds |^pound |^pounds |pounds$|pound$|^or | s$| e$| e | on |^s | of$| can$| lb$|",
                   " can$|lb$|^low | in$|^a | jar | oz$| room |^un | un$|^g |",
                   " kg$|^ml | at | caps$| temp$| in$|^ears | the | a | in$|",
                   " top | top$|^ap | ap | fat |^es | it | get | pints |^pints | pint |^pint|",
                   "^ed| as$| lg$|^new | new |^a |^low | in$| off$| pan$| one | two |^two | in$| bag | p |",
                   "^c |bob| s | add |^t |^ed | as$| ed | f$| ing$| big | es | |^es | extra | gs | g |^d | de$|",
                   " yes |skinned|probably| yes | cm |^fl oz| ml |^un |^v | is$| any |^tub |^the |^tb |^t ")

ingredient_list <- lapply(ingredient_list, function(x) gsub(details, " ", x))
ingredient_list <- lapply(ingredient_list, function(x) gsub(details2, " ", x))
ingredient_list <- lapply(ingredient_list, function(x) gsub(details, " ", x))
ingredient_list <- lapply(ingredient_list, function(x) gsub(details2, " ", x))
ingredient_list <- lapply(ingredient_list, function(x) gsub(details, " ", x))
ingredient_list <- lapply(ingredient_list, function(x) gsub(details2, " ", x))
ingredient_list <- lapply(ingredient_list, function(x) gsub(details, " ", x))
ingredient_list <- lapply(ingredient_list, function(x) gsub(details2, " ", x))
ingredient_list <- lapply(ingredient_list, function(x) gsub("^\\s+|\\s+$", "", x))
ingredient_list <- lapply(ingredient_list, function(x) gsub("   ", " ", x))
ingredient_list <- lapply(ingredient_list, function(x) gsub("  ", " ", x))
ingredient_list <- lapply(ingredient_list, function(x) gsub("  ", " ", x))
ingredient_list <- lapply(ingredient_list, function(x) gsub(" ", "\\.", x))

dat$ingredients <- ingredient_list

dat <- filter(dat, as.character(ingredients) != "character(0)")
dat <- filter(dat, as.character(nutritionEstimates) != "list()")

dat2 <- dat %>%
  unnest(ingredients)

dat3 <- dcast(dat2, ... ~ ingredients, function(x) 1, fill = 0)

dat3$nutritionEstimates <- dat$nutritionEstimates

dat4 <- dat3 %>%
  unnest(nutritionEstimates)

#rm(dat, dat2, dat3)

dat4 <- select(dat4, -one_of(tail(colnames(dat4),5)))

View(filter(dat, as.character(nutritionEstimates) == "list()"))

dat5 <- dcast(dat4, ... ~ attribute, value.var = "value")

nrow(dat)
nrow(dat5)

write.csv(dat5, file = "reshaped_data.csv", row.names = FALSE)
