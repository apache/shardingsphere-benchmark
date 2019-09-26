<template>
  <div id="app">
    <Container>
      <router-view />
    </Container>
  </div>
</template>

<script>
import Container from '@/components/Container'

export default {
  name: 'App',
  components: {
    Container
  }
}
</script>
