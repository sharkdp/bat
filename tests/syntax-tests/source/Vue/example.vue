<template>
  <div id="app" class="container-fluid">
    <AppHeader></AppHeader>
    <transition name="page" mode="out-in" v-if="!isLoading">
      <router-view></router-view>
    </transition>
    <AppLoadingIndicator></AppLoadingIndicator>
  </div>
</template>

<script>
import AppHeader from "@/components/AppHeader";
import AppLoadingIndicator from "@/components/AppLoadingIndicator";
import { mapGetters } from "vuex";

export default {
  name: "App",
  components: {
    AppHeader,
    AppLoadingIndicator,
  },
  beforeCreate() {
    this.$store.dispatch("fetchData");
  },
  data: {
    message: "Hello!"
  },
  computed: {
    ...mapGetters({
      isLoading: "isLoading",
    }),
  },
};
</script>

<style>
body {
  background-color: rgba(72, 163, 184, 0.05) !important;
}

.page-enter-active,
.page-leave-active {
  transition: opacity 0.2s;
}

.page-enter,
.page-leave-active {
  opacity: 0;
}

.page-enter:hover {
    opacity: 1;
}

</style>
