#include <string.h>

#include <bennet-exp/assign.h>
#include <bennet-exp/failure.h>

bennet_domain_failure_info bennet_domain_from_assignment(
    const void* p_alloc, const void* p, size_t bytes) {
  size_t lower_offset = (p < p_alloc) ? (p_alloc - p) : 0;
  size_t upper_offset = (p + bytes > p_alloc) ? ((p + bytes) - p_alloc) : 0;

  bennet_domain_failure_info domain = bennet_domain_default(intmax_t);
  domain.is_owned = true;
  domain.lower_offset_bound = lower_offset;
  domain.upper_offset_bound = upper_offset;

  return domain;
}

bool bennet_assign(void* id,
    cn_pointer* base_ptr,
    cn_pointer* addr,
    void* value,
    size_t bytes,
    const void* vars[]) {
  bennet_domain_failure_info domain;

  void* raw_base_ptr = convert_from_cn_pointer(base_ptr);
  if (raw_base_ptr == NULL) {
    bennet_failure_set_failure_type(BENNET_FAILURE_ASSIGN);

    domain = bennet_domain_failure_default();
    domain.upper_offset_bound = (bytes > sizeof(intmax_t)) ? bytes : sizeof(intmax_t);
    bennet_failure_blame_domain(id, &domain);

    return true;
  }

  void* raw_addr = convert_from_cn_pointer(addr);
  if (!bennet_alloc_check(raw_addr, bytes)) {
    bennet_failure_set_failure_type(BENNET_FAILURE_ASSIGN);

    domain = bennet_domain_from_assignment(raw_base_ptr, raw_addr, bytes);
    bennet_failure_blame_domain(id, &domain);

    return true;
  }

  if (!bennet_ownership_check(raw_addr, bytes)) {
    bennet_failure_set_failure_type(BENNET_FAILURE_ASSERT);

    bennet_failure_blame_many(vars);

    return true;
  }

  memcpy(raw_addr, value, bytes);
  bennet_ownership_update(raw_addr, bytes);

  return false;
}
