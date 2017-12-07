solve <- function (captcha = "1122") {
  nums <- as.numeric(strsplit(captcha,"")[[1]])
  print(nums)
  out <- 0
  for(i in 1:length(nums)-1) {
    if(nums[i+1] == nums[(i+1)%%length(nums) + 1]) {
      out <- out + nums[i+1]
    }
  }
  out
}

# STEP 2

solve2 <- function(captcha = "123425") {
  nums <- as.numeric(strsplit(captcha,"")[[1]])
  out <- 0
  for(i in 1:length(nums)-1) {
    if(nums[i+1] == nums[(i + (length(nums) %/% 2)) %% length(nums) + 1]) {
      out <- out + nums[i+1]
    }
  }
  out
}